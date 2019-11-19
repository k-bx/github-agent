module Le.Main where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time
import qualified Data.Time.Format
import qualified GitHub.Auth
import qualified GitHub.Data.Definitions
import qualified GitHub.Data.Id
import qualified GitHub.Data.Issues
import qualified GitHub.Endpoints.Issues
import Le.Import
import Options.Applicative
import qualified Prelude
import qualified System.Directory
import System.Process.Typed (proc, readProcess_, runProcess_, setWorkingDir)

data Config =
  Config
    { cfgOwnerName :: GitHub.Endpoints.Issues.Name GitHub.Endpoints.Issues.Owner
    , cfgRepoName :: GitHub.Endpoints.Issues.Name GitHub.Endpoints.Issues.Repo
    , cfgDataDir :: FilePath
    , cfgAuth :: Text -- GitHub.Endpoints.Issues.Auth
    }
  deriving (Generic)

instance J.FromJSON Config where
  parseJSON = J.genericParseJSON (jsonOpts 3)

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
  issues <-
    eitherSErr <$>
    GitHub.Endpoints.Issues.issuesForRepo'
      (Just (GitHub.Auth.OAuth (S.fromText cfgAuth)))
      cfgOwnerName
      cfgRepoName
      mempty
  forM_ issues $ \issue -> do
    let outfp =
          cfgDataDir <> "/" <>
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

gitStatus :: Config -> IO [StatusLine]
gitStatus Config {..} = do
  let readP_ = readProcess_ . setWorkingDir cfgDataDir
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
commitAll cfg@Config {..} = do
  logI $ "> Commiting changes"
  let runP_ = runProcess_ . setWorkingDir cfgDataDir
  entries <- gitStatus cfg
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
  entries <- gitStatus cfg
  forM_ entries $ \entry -> do
    case entry of
      StatusLineM fname -> do
        let issueId :: Int
            issueId = Prelude.read (S.toString (T.dropEnd 3 fname))
        logI $ "> Editing issue on GitHub: " <> show issueId
        newBody <- T.readFile (cfgDataDir <> "/" <> show issueId <> ".md")
        System.Directory.createDirectoryIfMissing False $
          cfgDataDir <> "/backup"
        logI $ "> Getting info and backing up"
        issue <-
          eitherSErr <$>
          GitHub.Endpoints.Issues.issue'
            (Just (GitHub.Auth.OAuth (S.fromText cfgAuth)))
            cfgOwnerName
            cfgRepoName
            (GitHub.Data.Id.Id issueId)
        t <- Data.Time.getCurrentTime
        let stamp =
              Data.Time.Format.formatTime
                Data.Time.Format.defaultTimeLocale
                "%F-%X"
                t
        let bakfp = cfgDataDir <> "/backup/" <> show issueId <> "-" <> stamp
        logI $ "> Writing backup in " <> bakfp
        T.writeFile bakfp (fromMaybe "" (GitHub.Data.Issues.issueBody issue))
        logI $ "> Doing the update"
        eitherSErr <$>
          GitHub.Endpoints.Issues.editIssue
            (GitHub.Auth.OAuth (S.fromText cfgAuth))
            cfgOwnerName
            cfgRepoName
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
  eitherSErr <$> (J.eitherDecodeFileStrict (h <> "/.github-agent.json"))

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
