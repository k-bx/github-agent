module Le.Main where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time
import qualified Data.Time.Format
import qualified Dhall
import qualified GitHub.Auth
import qualified GitHub.Data.Definitions
import qualified GitHub.Data.Id
import qualified GitHub.Data.Issues
import qualified GitHub.Data.Name
import qualified GitHub.Endpoints.Issues
import Le.Import
import Options.Applicative
import qualified Prelude
import qualified System.Directory
import System.Process.Typed (proc, readProcess_, runProcess_, setWorkingDir)

data Config =
  Config
    { repos :: [Repo]
    }
  deriving (Generic)

data Repo =
  Repo
    { repo_owner_name :: Text
    , repo_name :: Text
    , repo_data_dir :: FilePath
    , repo_auth :: Text
    }
  deriving (Generic)

instance Dhall.FromDhall Repo

instance Dhall.FromDhall Config

main :: IO ()
main = do
  join
    (customExecParser
       (prefs (showHelpOnError <> showHelpOnEmpty))
       (info (helper <*> hsubparser commands) imod))

imod :: InfoMod a
imod = fullDesc <> progDesc "GitHub Agent App"

commands :: Mod CommandFields (IO ())
commands =
  mempty <>
  cmd "sync-in" "Sync issues from GitHub to localhost" (pure syncIssuesIn) <>
  cmd "sync-out" "Sync issues from localhost to GitHub" (pure syncIssuesOut)

cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd n d p = command n (info p (progDesc d))

syncIssuesIn :: IO ()
syncIssuesIn = do
  cfg@Config {..} <- readConfig
  forM_ repos $ \Repo {..} -> do
    let runP_ = runProcess_ . setWorkingDir repo_data_dir
    exists <- System.Directory.doesDirectoryExist repo_data_dir
    when (not exists) $ do
      logI $ "> Creating and initialising the repo data dir: " <> repo_data_dir
      System.Directory.createDirectory repo_data_dir
      runP_ $ proc "git" ["init", "."]
    issues <-
      eitherSErr <$>
      GitHub.Endpoints.Issues.issuesForRepo'
        (Just (GitHub.Auth.OAuth (S.fromText repo_auth)))
        (GitHub.Data.Name.N repo_owner_name)
        (GitHub.Data.Name.N repo_name)
        mempty
    forM_ issues $ \issue -> do
      let outfp =
            repo_data_dir <> "/" <>
            show
              (GitHub.Data.Definitions.unIssueNumber
                 (GitHub.Data.Issues.issueNumber issue) :: Int) <>
            ".md"
      logI $ "> Writing: " <> outfp
      T.writeFile outfp (fromMaybe "" (GitHub.Data.Issues.issueBody issue))
  commitAll cfg

data StatusLine
  = StatusLineQ Text
  | StatusLineM Text

gitStatus :: Repo -> IO [StatusLine]
gitStatus Repo {..} = do
  let readP_ = readProcess_ . setWorkingDir repo_data_dir
  (out, _) <- readP_ $ (proc "git" ["status", "-z"])
  let entries =
        BL.split 0 out |> map S.toText |> filter ((/= "") . T.strip . S.toText) |>
        map (T.splitOn " ")
  logI $ show entries
  fmap catMaybes $
    forM entries $ \entry -> do
      case entry of
        ["??", fname] -> do
          pure $ Just $ StatusLineQ fname
        ["", "M", fname] -> do
          pure $ Just $ StatusLineM fname
        _ -> pure Nothing

commitAll :: Config -> IO ()
commitAll Config {..} = do
  forM_ repos $ \repo@Repo {..} -> do
    logI $ "> Commiting changes"
    let runP_ = runProcess_ . setWorkingDir repo_data_dir
    entries <- gitStatus repo
    forM_ entries $ \entry -> do
      case entry of
        StatusLineQ fname -> do
          when (T.takeEnd 3 fname == ".md") $ do
            runP_ $ proc "git" ["add", S.toString fname]
        StatusLineM fname -> do
          logI $ "> Adding modified: " <> S.toString fname
          runP_ $ proc "git" ["add", S.toString fname]
          pure ()
    logI $ "> Commiting"
    runP_ $ proc "git" ["commit", "-m", "sync", "."]

syncIssuesOut :: IO ()
syncIssuesOut = do
  cfg@Config {..} <- readConfig
  forM_ repos $ \repo@Repo {..} -> do
    entries <- gitStatus repo
    forM_ entries $ \entry -> do
      case entry of
        StatusLineM fname -> do
          let issueId :: Int
              issueId = Prelude.read (S.toString (T.dropEnd 3 fname))
          logI $ "> Editing issue on GitHub: " <> show issueId
          newBody <- T.readFile (repo_data_dir <> "/" <> show issueId <> ".md")
          System.Directory.createDirectoryIfMissing False $
            repo_data_dir <> "/backup"
          logI $ "> Getting info and backing up"
          issue <-
            eitherSErr <$>
            GitHub.Endpoints.Issues.issue'
              (Just (GitHub.Auth.OAuth (S.fromText repo_auth)))
              (GitHub.Data.Name.N repo_owner_name)
              (GitHub.Data.Name.N repo_name)
              (GitHub.Data.Id.Id issueId)
          t <- Data.Time.getCurrentTime
          let stamp =
                Data.Time.Format.formatTime
                  Data.Time.Format.defaultTimeLocale
                  "%F-%X"
                  t
          let bakfp =
                repo_data_dir <> "/backup/" <> show issueId <> "-" <> stamp
          logI $ "> Writing backup in " <> bakfp
          T.writeFile bakfp (fromMaybe "" (GitHub.Data.Issues.issueBody issue))
          logI $ "> Doing the update"
          eitherSErr <$>
            GitHub.Endpoints.Issues.editIssue
              (GitHub.Auth.OAuth (S.fromText repo_auth))
              (GitHub.Data.Name.N repo_owner_name)
              (GitHub.Data.Name.N repo_name)
              (GitHub.Data.Id.Id issueId)
              (GitHub.Data.Issues.EditIssue
                 { editIssueTitle = Nothing
                 , editIssueBody = Just newBody
                 , editIssueAssignees = Nothing
                 , editIssueState = Nothing
                 , editIssueMilestone = Nothing
                 , editIssueLabels = Nothing
                 })
          pure ()
        _ -> pure ()
  commitAll cfg

readConfig :: IO Config
readConfig = do
  h <- System.Directory.getHomeDirectory
  Dhall.input Dhall.auto (S.toText (h <> "/.github-agent.dhall"))

logI :: String -> IO ()
logI = Prelude.putStrLn

eitherSErr :: Show e => Either e c -> c
eitherSErr = either (error . show) id

jsonOpts :: Int -> J.Options
jsonOpts n =
  J.defaultOptions
    { J.fieldLabelModifier = J.camelTo2 '_' . drop n
    , J.constructorTagModifier = J.camelTo2 '_' . drop n
    }

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) a f = f a

infixl 0 |>
