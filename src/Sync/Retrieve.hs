module Sync.Retrieve where
import Data.OrgMode
import Data.Issue

import Control.Monad
import Data.Hashable
import Data.List
import Data.Maybe
import Debug.Trace (trace)

import qualified Network.Google.OAuth2 as OA
import qualified Data.HashMap.Strict as HM
import qualified Sync.Retrieve.Google.Code as GC
import qualified Sync.Retrieve.Google.Tasks as GT
import qualified Sync.Retrieve.GitHub.GitHub as GH

class LoadableSource s where
  isMember :: s -> Issue -> Bool
  fetchList :: Maybe String -> s -> IO ([Issue])
  fetchDetails :: Maybe String -> s -> Issue -> IO (Maybe Issue)

data GoogleCodeSource = GoogleCodeSource
                        { gcRepo :: String
                        , gcSearchTerms :: [String]
                        } deriving (Eq, Show)

instance LoadableSource GoogleCodeSource where
  isMember (GoogleCodeSource repo tags) iss =
    iType iss == "googlecode" && origin iss == repo
  fetchList oauth (GoogleCodeSource repo tags) =
    GC.fetch repo tags
  fetchDetails oauth (GoogleCodeSource repo tags) iss = do
    details <- GC.fetchDetails (origin iss) (number iss)
    return $ Just iss { events = details }

data GitHubSource = GitHubSource
                    { ghUser :: String
                    , ghProject :: String
                    , ghTags :: [String]
                    } deriving (Eq, Show)

instance LoadableSource GitHubSource where
  isMember ghs iss =
    iType iss == "github" &&
    (ghUser ghs ++ "/" ++ ghProject ghs) == origin iss
  fetchList oauth ghs =
    GH.fetch oauth (ghUser ghs) (ghProject ghs) (Just Open) (ghTags ghs)
  fetchDetails oauth ghs iss =
    liftM Just $ GH.fetchDetails oauth (ghUser ghs) (ghProject ghs) iss


data GoogleTasksSource = GoogleTasksSource
                         { gtAlias :: String
                         , gtUser :: String
                         , gtOAuthFile :: Maybe FilePath
                         , gtClient :: OA.OAuth2Client
                         , gtListPatterns :: [String]
                         } deriving (Show)

authClientEq a b = (OA.clientId a) == (OA.clientId b) &&
                   (OA.clientSecret a) == (OA.clientSecret b)

instance Eq GoogleTasksSource where
  (==) a b =
    (gtAlias a) == (gtAlias b) &&
    (gtUser a) == (gtUser b) &&
    (gtOAuthFile a) == (gtOAuthFile b) &&
    authClientEq (gtClient a) (gtClient b) &&
    (gtListPatterns a) == (gtListPatterns b)

instance LoadableSource GoogleTasksSource where
  isMember ts iss =
    iType iss == "google-tasks" && (gtUser ts == user iss)
  fetchList oauth gts = do
    -- This oauth arg is awful, and just for github.  ugh.
    -- TODO: replace with a larger type that can handle all our auth
    -- needs (including inheritance, etc), and then lookup what I need
    -- here.
    let auth = gtOAuthFile gts
        client = gtClient gts
    lists <- GT.getLists auth client (gtListPatterns gts)
    all <- mapM (\list -> GT.getList auth client (gtUser gts) list) lists
    return $ concat all
  fetchDetails auth gts iss =
    return $ Just iss

data IssueFile = IssueFile
                 { ifPath :: FilePath
                 , ifDoc :: OrgDocView Issue }

instance Eq IssueFile where
  a == b = ifPath a == ifPath b

instance Ord IssueFile where
  compare a b = compare (ifPath a) (ifPath b)

instance Show IssueFile where
  show = ifPath


-- |We get issues from multiple sources, track that, and their states.
data InputIssue = FetchedIssue { fiIssue :: Issue } -- ^From a network source.
                  -- |An issue loaded from an org file.
                | LoadedIssue { liUpdate:: Bool
                                -- ^Whether to update
                              , liFile:: IssueFile
                              , liIssue:: Issue }
                  -- |An issue that was loaded from an org file, then
                  -- updated from the network.
                | MergedIssue { fetchedIssue :: Issue
                              , loadedIssue :: Issue
                              , loadedFile :: IssueFile
                              , loadedUpdate :: Bool
                                -- ^If it's been updated with details.
                              }

issueOf :: InputIssue -> Issue
issueOf (FetchedIssue fi) = fi
issueOf (LoadedIssue _ _ is) = is
issueOf (MergedIssue fi _ _ _) = fi

showIssue (Issue _ n _ _ _ _ _ es) =
  (show n) ++ "/" ++ (show $ length es)

instance Show InputIssue where
  show (FetchedIssue f) = "F<" ++ (showIssue f) ++ ">"
  show (LoadedIssue u fl is) =
    "L<" ++ upd ++ "," ++ (showIssue is) ++ ">"
    where upd = if u then "y" else "n"
  show (MergedIssue f l _ u) =
    "M[F<" ++ (showIssue f) ++ ">, L<" ++ (showIssue l) ++ "> " ++ upd ++ "]"
    where upd = if u then "U" else "_"

instance Hashable InputIssue where
  hashWithSalt i s = hashWithSalt i (issueOf s)

instance Eq InputIssue where
  a == b = issueOf a == issueOf b

eqIssue a b = issueOf a == issueOf b

instance Ord InputIssue where
  compare a b = compare (issueOf a) (issueOf b)


-- |Updates an existing issue in a set of them, merging in |iss|.
updateSet :: HM.HashMap Issue InputIssue -> InputIssue -> HM.HashMap Issue InputIssue
updateSet set iss =
  let kIss = issueOf iss
      -- Combine an issue we found in this fetch run with one we found
      -- before in a file load.  We shouldn't get Merged or Fetched
      -- issues on the RHS, as they should've been filtered out before
      -- we got here.
      mergeIssue (FetchedIssue newIss) (LoadedIssue upd file oldIss) =
        MergedIssue newIss oldIss file upd
  in case HM.lookup kIss set of
    Just oldIss -> HM.insert kIss (mergeIssue iss oldIss) set
    Nothing -> set

wantDetails (FetchedIssue _) = True
wantDetails (MergedIssue _ _ _ upd) = not upd
-- So, these are issues we didn't find in a fetch.  Do we want
-- to keep getting any details on them at all?  If they're in a
-- stub file, we may still want to know if they're closed,
-- right?  Yes.
wantDetails li@(LoadedIssue upd _ iss) =
  upd && wantAnyUpdate li

-- Whether we should even touch the file with the issue in it.  This
-- is true for everything but a file with an :ARCHIVE: tag on it
wantAnyUpdate (LoadedIssue _ _ iss) =
  not ("ARCHIVE" `elem` (tags iss))
wantAnyUpdate (MergedIssue _ _ _ True) = False
wantAnyUpdate _ = True

mergeFetchedWithLoaded :: [InputIssue] -> [InputIssue] -> [InputIssue]
mergeFetchedWithLoaded existing new =
  let pairedIssues = map (\i -> (issueOf i, i)) existing
  in map snd $ HM.toList $ foldl updateSet (HM.fromList pairedIssues) new

loadGCSource :: [InputIssue] -> GoogleCodeSource -> IO ([InputIssue])
loadGCSource existing src = do
  let (GoogleCodeSource repo terms) = src
  loaded <- GC.fetch repo terms
  return $ mergeFetchedWithLoaded existing $ map FetchedIssue loaded

previouslyFetched (FetchedIssue _) = True
previouslyFetched (MergedIssue _ _ _ _) = True
previouslyFetched _ = False

fetchIssue :: (LoadableSource s) => s -> Maybe String -> InputIssue -> IO (InputIssue)
fetchIssue s oauth (FetchedIssue iss) = do
  updIss <- fetchDetails oauth s iss
  return $ FetchedIssue (fromJust updIss)
fetchIssue s oauth li@(LoadedIssue False _ _) = do
  -- Don't update, just return what we took in.
  return li
fetchIssue s oauth (LoadedIssue True file iss) = do
  updIss <- fetchDetails oauth s iss
  return $ MergedIssue (fromJust updIss) iss file True
fetchIssue s oauth (MergedIssue fi ld fl False) = do
  updIss <- fetchDetails oauth s fi
  return $ MergedIssue (fromJust updIss) (fromJust updIss) fl True
fetchIssue s oauth mi@(MergedIssue fi ld fl True) = do
  putStrLn $ "Warning: attempted to fetch a previously-updated issue: " ++ (
    show ld)
  return mi

-- | Load data from a source, given an |oauth| token, a |src|, and a
-- list of |allExisting| values.  Assumes that any issue in
-- allExisting that should have been updated is already updated.
-- Returns an updated version of |allExisting| with MergedIssue
-- elements replacing LoadedIssues when we have new data, and
-- FetchedIssue added for new issues.
loadSource :: (LoadableSource s) => Maybe String -> [InputIssue] -> s -> IO ([InputIssue])
loadSource oauth allExisting src = do
  -- Generally, get a list of issues from fetchList, and fetchDetails on them.
  -- Ignore to the get-details list:
  --  Any in the list that were in allExisting and previouslyFetched.
  -- Add to the get-details list:
  --  Any that we saw in allExisting that were both relevant and not previouslyFetched.
  let (existing, nonMembers) = partition (\i -> isMember src $ issueOf i) allExisting
      (loadedExisting, unloadedExisting) = partition previouslyFetched existing
  incoming <- mapM (return . FetchedIssue) =<< fetchList oauth src
  let new = incoming \\ loadedExisting
      orphans = unloadedExisting
  fetchedNew <- mapM (fetchIssue src oauth) new
  fetchedOrphans <- mapM (fetchIssue src oauth) orphans
  return (nonMembers ++ loadedExisting ++ fetchedNew ++ fetchedOrphans)
