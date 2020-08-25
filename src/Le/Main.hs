module Le.Main where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time
import qualified Data.Time.Format
import qualified Data.Vector as V
import qualified Dhall
import qualified GitHub as GH
import qualified GitHub.Auth
import qualified GitHub.Data.Definitions
import qualified GitHub.Data.Issues
import qualified GitHub.Data.Name
import qualified GitHub.Endpoints.Issues
import qualified GitLab
import Le.Import
import Options.Applicative
import qualified Safe
import qualified System.Directory
import System.Process.Typed (proc, readProcess_, runProcess_, setWorkingDir)
import qualified Prelude

data Config = Config
  { repos :: [Repo]
  }
  deriving (Generic)

data RepoSource = Github | Gitlab deriving (Generic)

instance Dhall.FromDhall RepoSource

data Repo = Repo
  { repo_owner_name :: Text,
    repo_name :: Text,
    repo_source :: RepoSource,
    repo_data_dir :: FilePath,
    repo_auth :: Text
  }
  deriving (Generic)

instance Dhall.FromDhall Repo

instance Dhall.FromDhall Config

type IssueId = Int

main :: IO ()
main = do
  join
    ( customExecParser
        (prefs (showHelpOnError <> showHelpOnEmpty))
        (info (helper <*> hsubparser commands) imod)
    )

imod :: InfoMod a
imod = fullDesc <> progDesc "GitHub Agent App"

commands :: Mod CommandFields (IO ())
commands =
  mempty
    <> cmd "sync-in" "Sync issues from GitHub to localhost" (pure syncIssuesIn)
    <> cmd "sync-out" "Sync issues from localhost to GitHub" (pure syncIssuesOut)

cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd n d p = command n (info p (progDesc d))

syncIssuesIn :: IO ()
syncIssuesIn = do
  cfg@Config {..} <- readConfig
  forM_ repos $ \repo@Repo {..} -> do
    logI $ "> sync-in for repo: " <> repo_data_dir
    let runP_ = runProcess_ . setWorkingDir repo_data_dir
    exists <- System.Directory.doesDirectoryExist repo_data_dir
    when (not exists) $ do
      logI $ "> Creating and initialising the repo data dir: " <> repo_data_dir
      System.Directory.createDirectory repo_data_dir
      runP_ $ proc "git" ["init", "."]
    issues <- getIssues repo
    entries <- gitStatus repo
    (modifiedEntries :: [IssueId]) <-
      fmap catMaybes $
        forM
          entries
          ( \case
              StatusLineM fpath -> fmap Just $ filenameToIssueId fpath
              _ -> pure Nothing
          )
    forM_ issues $ \(issueNum, IssueInfo {..}) -> do
      let outfp = repo_data_dir <> "/" <> show issueNum <> ".md"
      outfpExists <- System.Directory.doesFileExist outfp
      when outfpExists $ do
        outfpContents <- T.readFile outfp
        when (outfpContents /= infBody) $ do
          logI $ "> Writing: " <> outfp
      case (issueNum `elem` modifiedEntries) of
        True -> do
          logI $
            "> Skipping sync-in for a locally modified issue: " <> show issueNum
          tmpDir <- System.Directory.getTemporaryDirectory
          let remoteContentsFp = tmpDir <> "/" <> show issueNum <> "-remote.txt"
          T.writeFile remoteContentsFp infBody
          runP_ $
            proc
              "git"
              [ "diff",
                "--no-index",
                "--word-diff=color",
                remoteContentsFp,
                outfp
              ]
        False -> do
          T.writeFile outfp infBody
  commitAll cfg

syncIssuesOut :: IO ()
syncIssuesOut = do
  cfg@Config {..} <- readConfig
  forM_ repos $ \repo@Repo {..} -> do
    logI $ "> sync-out for repo: " <> repo_data_dir
    let readP_ = readProcess_ . setWorkingDir repo_data_dir
    let runP_ = runProcess_ . setWorkingDir repo_data_dir
    entries <- gitStatus repo
    forM_ entries $ \entry -> do
      case entry of
        StatusLineM fname -> do
          issueId <- filenameToIssueId fname
          logI $ "> Editing issue on GitHub/GitLab: " <> show issueId
          let mdFilename = show issueId <> ".md"
          let mdFp = repo_data_dir <> "/" <> mdFilename
          newBody <- T.readFile mdFp
          System.Directory.createDirectoryIfMissing False $
            repo_data_dir <> "/backup"
          logI $ "> Getting info and backing up"
          issueInfo <- getIssueInfo repo issueId
          (originalContent_, _) <- readP_ $ proc "git" ["show", "HEAD:" <> mdFilename]
          let originalContent = S.toText originalContent_
          let newContent = infBody issueInfo
          case T.strip originalContent == T.strip newContent of
            False -> do
              logI $
                "> Skipping sync-out for a remotely modified issue: "
                  <> show issueId
              tmpDir <- System.Directory.getTemporaryDirectory
              let originalContentsFp =
                    tmpDir <> "/" <> show issueId <> "-original.txt"
              T.writeFile originalContentsFp originalContent
              let remoteContentsFp =
                    tmpDir <> "/" <> show issueId <> "-remote.txt"
              T.writeFile remoteContentsFp newContent
              runP_ $
                proc
                  "git"
                  [ "diff",
                    "--no-index",
                    "--word-diff=color",
                    originalContentsFp,
                    remoteContentsFp
                  ]
            -- runP_ $
            --   proc
            --     "git"
            --     [ "diff",
            --       "--no-index",
            --       "--word-diff=color",
            --       remoteContentsFp,
            --       mdFp
            --     ]
            True -> do
              t <- Data.Time.getCurrentTime
              let stamp =
                    Data.Time.Format.formatTime
                      Data.Time.Format.defaultTimeLocale
                      "%F-%X"
                      t
              let bakfp =
                    repo_data_dir <> "/backup/" <> show issueId <> "-" <> stamp
              logI $ "> Writing backup in " <> bakfp
              T.writeFile bakfp newContent
              logI $ "> Doing the update"
              updateIssue repo issueId newBody
              pure ()
        _ -> pure ()
  commitAll cfg

data StatusLine
  = StatusLineQ Text
  | StatusLineM Text

gitStatus :: Repo -> IO [StatusLine]
gitStatus Repo {..} = do
  let readP_ = readProcess_ . setWorkingDir repo_data_dir
  (out, _) <- readP_ $ (proc "git" ["status", "-z"])
  let entries =
        BL.split 0 out |> map S.toText |> filter ((/= "") . T.strip . S.toText)
          |> map (T.splitOn " ")
  -- logI $ show entries
  fmap catMaybes $
    forM entries $
      \entry -> do
        case entry of
          ["??", fname] -> do
            pure $ Just $ StatusLineQ fname
          ["", "M", fname] -> do
            pure $ Just $ StatusLineM fname
          _ -> pure Nothing

commitAll :: Config -> IO ()
commitAll Config {..} = do
  forM_ repos $ \repo@Repo {..} -> do
    let runP_ = runProcess_ . setWorkingDir repo_data_dir
    entries <- gitStatus repo
    needCommit <- fmap (any (== True)) $
      forM entries $ \entry -> do
        case entry of
          StatusLineQ fname -> do
            case (T.takeEnd 3 fname == ".md") of
              True -> do
                runP_ $ proc "git" ["add", S.toString fname]
                pure True
              _ -> pure False
          StatusLineM fname -> do
            logI $ "> Adding modified: " <> S.toString fname
            runP_ $ proc "git" ["add", S.toString fname]
            pure True
    when needCommit $ do
      logI $ "> commiting changes for repo: " <> repo_data_dir
      runP_ $ proc "git" ["commit", "-m", "sync", "."]

-- | Partial
filenameToIssueId :: Text -> IO IssueId
filenameToIssueId fname = do
  let issueId :: IssueId
      issueId = Prelude.read (S.toString (T.dropEnd 3 fname))
  pure issueId

data IssueInfo = IssueInfo {infBody :: Text}
  deriving (Generic)

getIssues :: Repo -> IO (Vector (IssueId, IssueInfo))
getIssues repo@Repo {..} = do
  case repo_source of
    Github -> do
      (issues :: Vector GH.Issue) <-
        -- (eitherSErr :: Either GH.Error (Vector GH.Issue) -> Vector GH.Issue)
        (eitherSErr @GH.Error)
          <$> GH.github
            (GitHub.Auth.OAuth (S.fromText repo_auth))
            GitHub.Endpoints.Issues.issuesForRepoR
            (GitHub.Data.Name.N repo_owner_name)
            (GitHub.Data.Name.N repo_name)
            mempty
            GH.FetchAll
      forM issues $ \issue -> do
        let issueNum :: IssueId
            issueNum =
              GitHub.Data.Definitions.unIssueNumber
                (GitHub.Data.Issues.issueNumber issue)
        let issueBody = fromMaybe "" (GitHub.Data.Issues.issueBody issue)
        pure (issueNum, IssueInfo {infBody = issueBody})
    Gitlab -> do
      proj <- getGitlabProj repo
      let gitlabConf = gitlabConfig repo
      issues <-
        GitLab.runGitLab gitlabConf $
          GitLab.projectOpenedIssues proj
      fmap V.fromList $
        forM issues $ \issue -> do
          pure
            ( GitLab.iid issue,
              IssueInfo {infBody = fromMaybe "" (GitLab.issue_description issue)}
            )

gitlabConfig :: Repo -> GitLab.GitLabServerConfig
gitlabConfig Repo {..} =
  GitLab.defaultGitLabServer {GitLab.token = repo_auth}

getGitlabProj :: Repo -> IO GitLab.Project
getGitlabProj repo@Repo {..} = do
  let gitlabConf = gitlabConfig repo
  GitLab.runGitLab
    gitlabConf
    ( GitLab.projectsWithNameAndUser
        repo_owner_name
        repo_name
    )
    & fmap eitherSErr
    & fmap (Safe.fromJustNote (S.toString ("couldn't find a project: " <> repo_owner_name <> "/" <> repo_name)))

getIssueInfo :: Repo -> IssueId -> IO IssueInfo
getIssueInfo repo@Repo {..} issueId = do
  case repo_source of
    Github -> do
      issue <-
        (eitherSErr @GH.Error)
          <$> GH.github
            (GitHub.Auth.OAuth (S.fromText repo_auth))
            GitHub.Endpoints.Issues.issueR
            (GitHub.Data.Name.N repo_owner_name)
            (GitHub.Data.Name.N repo_name)
            (GH.IssueNumber issueId)
      pure $ IssueInfo {infBody = fromMaybe "" (GitHub.Data.Issues.issueBody issue)}
    Gitlab -> do
      proj <- getGitlabProj repo
      let gitlabConf = gitlabConfig repo
      issues <-
        GitLab.runGitLab gitlabConf $
          GitLab.projectOpenedIssues proj
      let issue =
            issues
              & filter (\x -> GitLab.iid x == issueId)
              & listToMaybe
              & Safe.fromJustNote (S.toString ("couldn't find an issue: " <> tshow @IssueId issueId))
      pure $ IssueInfo {infBody = fromMaybe "" (GitLab.issue_description issue)}

updateIssue :: Repo -> IssueId -> Text -> IO ()
updateIssue repo@Repo {..} issueId newBody = do
  case repo_source of
    Github -> do
      void $
        (eitherSErr @GH.Error)
          <$> GH.github
            (GitHub.Auth.OAuth (S.fromText repo_auth))
            GitHub.Endpoints.Issues.editIssueR
            (GitHub.Data.Name.N repo_owner_name)
            (GitHub.Data.Name.N repo_name)
            (GH.IssueNumber issueId)
            ( GitHub.Data.Issues.EditIssue
                { editIssueTitle = Nothing,
                  editIssueBody = Just newBody,
                  editIssueAssignees = Nothing,
                  editIssueState = Nothing,
                  editIssueMilestone = Nothing,
                  editIssueLabels = Nothing
                }
            )
    Gitlab -> do
      proj <- getGitlabProj repo
      let editReq :: GitLab.EditIssueReq
          editReq =
            GitLab.EditIssueReq
              { edit_issue_id = (GitLab.project_id proj),
                edit_issue_issue_iid = issueId,
                edit_issue_title = Nothing,
                edit_issue_description = Just newBody,
                edit_issue_confidential = Nothing,
                edit_issue_assignee_ids = Nothing,
                edit_issue_milestone_id = Nothing,
                edit_issue_labels = Nothing,
                edit_issue_state_event = Nothing,
                edit_issue_updated_at = Nothing,
                edit_issue_due_date = Nothing,
                edit_issue_weight = Nothing,
                edit_issue_discussion_locked = Nothing,
                edit_issue_epic_id = Nothing,
                edit_issue_epic_iid = Nothing
              }
      let gitlabConf = gitlabConfig repo
      GitLab.runGitLab
        gitlabConf
        (GitLab.editIssue (GitLab.project_id proj) issueId editReq)
        & fmap eitherSErr
      pure ()

readConfig :: IO Config
readConfig = do
  h <- System.Directory.getHomeDirectory
  cfg <- Dhall.input Dhall.auto (S.toText (h <> "/.github-agent.dhall"))
  let replace_h rd =
        S.toString (T.replace "~" (S.toText h) (S.toText rd))
  let repos2 = map (\r -> r {repo_data_dir = replace_h (repo_data_dir r)}) (repos cfg)
  let cfg2 = cfg {repos = repos2}
  pure cfg2

logI :: String -> IO ()
logI = Prelude.putStrLn

eitherSErr :: Show e => Either e c -> c
eitherSErr = either (error . show) id

jsonOpts :: Int -> J.Options
jsonOpts n =
  J.defaultOptions
    { J.fieldLabelModifier = J.camelTo2 '_' . drop n,
      J.constructorTagModifier = J.camelTo2 '_' . drop n
    }

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) a f = f a

infixl 0 |>
