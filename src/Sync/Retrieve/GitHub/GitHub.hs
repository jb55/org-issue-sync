{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sync.Retrieve.GitHub.GitHub where
import Control.Exception
import Data.Issue
import Data.Maybe
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Char
import Data.Proxy (Proxy(..))
import Data.List (sort)
import Data.Monoid ((<>))
import Debug.Trace
import Network.HTTP.Conduit (HttpException(..))
import Network.HTTP.Types (statusCode, statusMessage)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified GitHub.Auth as GH
import qualified GitHub.Data.Issues as GH
import qualified GitHub.Data.Id as GH
import qualified GitHub.Data.Name as GH
import qualified GitHub.Endpoints.Issues as GH
import qualified GitHub.Endpoints.Issues.Events as GH
import qualified GitHub.Endpoints.Issues.Comments as GH
import qualified GitHub.Data.Definitions as GH

rstrip xs = reverse $ lstrip $ reverse xs
lstrip = dropWhile (== ' ')
strip xs = lstrip $ rstrip xs

convertIssue :: Text -> GH.Issue -> Issue
convertIssue origin iss =
  let user = fromMaybe (GH.issueUser iss) (GH.issueAssignee iss)
      userName = GH.simpleUserLogin user
      tags = fmap GH.labelName (GH.issueLabels iss)
      isClosed = isJust $ GH.issueClosedAt iss
      isActive = elem "T:Active" tags
      status
        | isClosed  = Closed
        | isActive  = Active
        | otherwise = Open
      cleanChar c
        | isAlphaNum c = c
        | otherwise = '_'
      cleanTag = T.map cleanChar
      cleanTags = V.toList (fmap cleanTag tags)
      nr = GH.issueNumber iss
      url = "https://www.github.com/" <> origin <> "/issues/" <> T.pack (show nr)
  in Issue origin nr (simpleName user) status cleanTags
      (T.strip $ GH.issueTitle iss) "github" url []

simpleName :: GH.SimpleUser -> Text
simpleName = GH.untagName . GH.simpleUserLogin

wrapEvent :: GH.Event -> IssueEventDetails -> IssueEvent
wrapEvent event =
  IssueEvent (GH.eventCreatedAt event)
             (simpleName $ GH.eventActor event)

convertEvent :: GH.Event -> IssueEventDetails
convertEvent = IssueComment . T.pack . show . GH.eventType

convertIssueEvent :: GH.Event -> [IssueEvent]
convertIssueEvent event
-- status change
  | GH.eventType event == GH.Assigned = [
    wrapEvent event $ IssueOwnerChange (simpleName (GH.eventActor event))]
  | GH.eventType event == GH.Closed = [
    wrapEvent event $ IssueStatusChange Closed]
  | GH.eventType event == GH.ActorUnassigned = [
    wrapEvent event $ IssueComment "Unassigned owner"]
  | GH.eventType event == GH.Reopened = [
    wrapEvent event $ IssueStatusChange Open]
  | GH.eventType event == GH.Renamed = [
    wrapEvent event $ IssueComment "Changed title"]
-- label change
  | GH.eventType event == GH.Labeled = [
    wrapEvent event $ IssueComment "Added a label"]
  | GH.eventType event == GH.Unlabeled = [
    wrapEvent event $ IssueComment "Removed a label"]
-- milestone change
  | GH.eventType event == GH.Milestoned =
    let mstone =
          case GH.eventIssue event of
            Just evt ->
              case GH.issueMilestone evt of
                Just ms -> ' ' `T.cons` GH.milestoneTitle ms
                Nothing -> ""
            Nothing -> ""
    in [wrapEvent event $ IssueComment ("Added milestone" <> mstone)]

  | GH.eventType event == GH.Demilestoned = [
    wrapEvent event $ IssueComment "Removed a milestone"]
  | GH.eventType event == GH.Subscribed = [
    wrapEvent event $ IssueComment "Subscribed"]
  | GH.eventType event == GH.Mentioned = [
    wrapEvent event $ IssueComment "Mentioned"]
-- ignored, make into comment
  | otherwise = [wrapEvent event $ IssueComment (T.pack $ show $ GH.eventType event)]

convertIssueComment :: GH.IssueComment -> [IssueEvent]
convertIssueComment comment =
  [IssueEvent (GH.issueCommentCreatedAt comment) (
      GH.untagName $ GH.simpleUserLogin $ GH.issueCommentUser comment) (
      IssueComment (GH.issueCommentBody comment))]

loadIssueComments :: Maybe GH.Auth
                  -> GH.Name GH.Owner
                  -> GH.Name GH.Repo
                  -> GH.Id GH.Issue
                  -> IO [IssueEvent]
loadIssueComments oauth usr repo num = do
  res <- GH.comments' oauth usr repo num
  case res of
    Left err -> do
      T.putStrLn (ownerRepo usr repo <> ": issue " <>
                   T.pack (show num) <> ": " <> T.pack (show err))
      return []
    Right comments ->
      return $ concatMap convertIssueComment comments

loadIssueEvents :: Maybe GH.Auth
                -> GH.Name GH.Owner
                -> GH.Name GH.Repo
                -> GH.Id GH.Issue
                -> IO [IssueEvent]
loadIssueEvents oauth user repo issnum = do
  let classifyError (GH.HTTPError ex) =
        case ex of
          StatusCodeException st _ _ -> "HTTP Connection Error "
                                       <> T.pack (show (statusCode st))
                                       <> ": "
                                       <> T.pack (show (statusMessage st))
          _ -> "HTTP Connection Error (unknown status code): "
            <> T.pack (show ex)
      classifyError err = T.pack (show err)
  res <- GH.eventsForIssue' oauth user repo issnum
  case res of
    Left err -> do
      T.putStrLn (ownerRepo user repo <> ": issue " <>
                   T.pack (show issnum) <> ": " <> classifyError err)
      return []
    Right events ->
      return $ concatMap convertIssueEvent events

makeIssueComment :: GH.Issue -> IssueEvent
makeIssueComment issue =
  let user = fromMaybe (GH.issueUser issue) (GH.issueAssignee issue)
      userName = GH.simpleUserLogin user
      createDate = GH.issueCreatedAt issue
  in IssueEvent createDate (GH.untagName userName)
         (IssueComment (fromMaybe "" (GH.issueBody issue)))

fetchIssue :: Maybe ByteString
           -> GH.Name GH.Owner
           -> GH.Name GH.Repo
           -> GH.Id GH.Issue
           -> IO (Maybe Issue)
fetchIssue tok ghuser@(GH.N user) ghrepo@(GH.N repo) issuenum = do
  let auth = fmap GH.OAuth tok
  res <- GH.issue' auth ghuser ghrepo issuenum
  case res of
    Left err -> do print err; return Nothing
    Right issue -> return $ Just $ convertIssue (user <> "/" <> repo ) issue


fetchDetails :: Maybe ByteString
             -> GH.Name GH.Owner
             -> GH.Name GH.Repo
             -> Issue
             -> IO Issue
fetchDetails tok user repo issue = do
  let auth = fmap GH.OAuth tok
      issuenum = GH.mkId (Proxy :: Proxy GH.Issue) (number issue)
  eventList <- loadIssueEvents auth user repo issuenum
  commentList <- loadIssueComments auth user repo issuenum
  -- assume that the issue already has the initial comment.
  return $ issue { events = events issue ++ (sort eventList ++ commentList) }

ownerRepo :: GH.Name GH.Owner -> GH.Name GH.Repo -> Text
ownerRepo (GH.N user) (GH.N repo) = user <> "/" <> repo

fetch :: Maybe ByteString
      -> GH.Name GH.Owner
      -> GH.Name GH.Repo
      -> Maybe IssueStatus
      -> [String]
      -> IO [Issue]
fetch tok ghuser@(GH.N user) ghrepo@(GH.N repo) stat tags = do
  let auth = fmap GH.OAuth tok
      statusLim = case stat of
        Just Open -> [GH.Open]
        Just Closed -> [GH.OnlyClosed]
        _ -> []
      tagLim = [GH.Labels tags | not (null tags)]
  res <- GH.issuesForRepo' auth ghuser ghrepo (statusLim++tagLim)
  case res of
    Left err -> do
      print err
      return []
    Right issues -> do
--      eventList <- mapM (\is -> loadIssueEvents auth user repo $ GH.issueNumber is) issues
--      commentList <-
--        mapM (\i -> loadIssueComments auth user repo (GH.issueNumber i)) issues
      let convertedIssues = fmap (convertIssue (ownerRepo ghuser ghrepo)) issues
          comments = fmap makeIssueComment issues
          conversions = V.zip convertedIssues comments
      return $ V.toList $ fmap (\(i,comm) -> i { events = [comm] }) conversions
