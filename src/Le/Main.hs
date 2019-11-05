module Le.Main where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GitHub.Auth
import qualified GitHub.Data.Definitions
import qualified GitHub.Data.Issues
import qualified GitHub.Endpoints.Issues
import Le.Import
import Options.Applicative
import qualified Prelude
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
  Config {..} <- readConfig
  when False $ do
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
  logI $ "> Commiting changes"
  let runP_ = runProcess_ . setWorkingDir cfgDataDir
      readP_ = readProcess_ . setWorkingDir cfgDataDir
  runP_ (proc "git" ["status", "--porcelain=v1"])
  (out, _) <- readP_ $ (proc "git" ["status", "-z"])
  let entries :: [[Text]]
      entries =
        BL.split 0 out |> map S.toText |> filter ((/= "") . T.strip . S.toText) |>
        map (T.splitOn " ")
  logI $ show entries
  forM_ entries $ \entry -> do
    case entry of
      ["", "M", fname] -> do
        logI $ "> Adding modified: " <> S.toString fname
        runP_ $ proc "git" ["add", S.toString fname]
        pure ()
      _ -> pure ()
  logI $ "> Commiting"
  runP_ $ proc "git" ["commit", "-m", "sync", "."]

syncIssuesOut :: IO ()
syncIssuesOut = undefined

readConfig :: IO Config
readConfig = eitherSErr <$> J.eitherDecodeFileStrict "config.json"

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
