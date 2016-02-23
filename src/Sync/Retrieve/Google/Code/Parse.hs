{-# LANGUAGE OverloadedStrings #-}

module Sync.Retrieve.Google.Code.Parse (parseIssueText) where

import Data.OrgMode.Text
import Text.HTML.TagSoup
import Text.StringLike
import Debug.Trace
import Data.Maybe
import Data.Issue
import Data.Time.Calendar
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Locale.Compat
import qualified Data.Time.Format as TF
import qualified Data.Text as T

data DepthTaggedTag str = DepthTaggedTag Int (Tag str) deriving (Eq)

instance (Show str) => Show (DepthTaggedTag str) where
  show (DepthTaggedTag depth tag) =
    (take depth $ repeat ' ') ++ show tag

-- | Problem: the tag stream can be bad.  We're seeing unclosed <br>
-- tags.  Unsurprising, but we must repair it.  So, if we maintain a
-- stack: [div div div b] and see a /div, we should pop the stack
-- backwards until we see the matching tag.
matchDepth :: [Tag Text] -> [Tag Text] -> ([DepthTaggedTag Text], [Tag Text])
matchDepth [] remain = ([], remain)
matchDepth stack [] = ([], [])
matchDepth stack (t:ts) =
  let depth = length stack
      indent = (take depth $ repeat ' ')
      matchesTag s (TagOpen n _) = s == n
      doesntMatchTag s (TagOpen n _) = s /= n
  in case t of
    TagOpen n _ ->
      let (tl, rem) = matchDepth (t:stack) ts
      in ((DepthTaggedTag depth t):tl, rem)
    TagClose n ->
      let new_stack =
            let fixed = dropWhile (doesntMatchTag n) stack
            in if length fixed > 0
               then tail fixed
               else []
          (tl, rem) = matchDepth new_stack ts
      in ((DepthTaggedTag (length new_stack) t):tl, rem)
    otherwise ->
      let (tl, rem) = matchDepth stack ts
      in ((DepthTaggedTag depth t):tl, rem)

-- | Return the open/close pair of tags (and include all the ones in
-- between) that match the pattern.
matchExternalTag :: (TagRep t) => t -> [Tag Text] -> ([Tag Text], [Tag Text])
matchExternalTag pattern input =
  let startOfStream = dropWhile (~/= pattern) input
      (found, rem) = matchDepth [head startOfStream] (tail startOfStream)
      stripDepth (DepthTaggedTag _ tag) = tag
  in if length startOfStream > 0
     then ((head startOfStream):(map stripDepth found), rem)
     else ([], [])

extractHistoryTags :: [Tag Text] -> [Tag Text]
extractHistoryTags tags =
  fst $ matchExternalTag ("<td class=\"vt issuedescription\"" :: String) tags

takeMatchingChildren pat [] = []
takeMatchingChildren pat tgs =
  let (chld, dropped) = matchExternalTag pat tgs
  in chld:(takeMatchingChildren pat dropped)

takeDivChildren :: [Tag Text] -> [[Tag Text]]
takeDivChildren tgs = takeMatchingChildren ("<div" :: String) tgs

-- | Takes output from extractHistoryTags and separates them out into individual groups of tags
separateChildrenDivs tags =
  filter (\l -> length l > 0) $ takeDivChildren tags

parseIssueDescription :: [Tag Text] -> (Text, Text)
parseIssueDescription tags =
  let user = innerText $ take 3 $ dropWhile (~/= ("<a class=userlink" :: String)) $ tags
      desc = innerText $ take 3 $ dropWhile (~/= ("<pre" :: String)) $ tags
  in (user, T.strip desc)

scanStatusChange :: UTCTime -> Text -> Text -> IssueEvent
scanStatusChange when user value =
  let newStat = case T.strip value of
        "Started" -> Active
        "Fixed" -> Closed
        otherwise -> Open
      statChange = IssueStatusChange newStat
  in IssueEvent when user statChange

scanOwnerChange :: UTCTime -> Text -> Text -> IssueEvent
scanOwnerChange when user value =
  let statChange = IssueOwnerChange (T.strip value)
  in IssueEvent when user statChange

scanLabelChange :: UTCTime -> Text -> Text -> IssueEvent
scanLabelChange when user value =
  let unsub :: Text -> Bool
      unsub s = T.head s == '-'
      newLabels = filter (\x -> not . unsub $ x) $ T.words value
      oldLabels = map T.tail $ filter unsub $ T.words value
      labelChange = IssueLabelChange newLabels oldLabels
  in IssueEvent when user labelChange

scanUpdateBox :: UTCTime -> Text -> [Tag Text] -> [Maybe IssueEvent]
scanUpdateBox when user [] = []
scanUpdateBox when user tags =
  let stat_update_text = "Status:"
      owner_update_text = "Owner:"
      label_update_text = "Labels:"
      (h, t) = matchExternalTag ("<b" :: String) tags
      key = T.strip $ innerText h
      value = T.strip $ innerText $ takeWhile (~/= ("<br" :: String)) t
      rest = dropWhile (~/= ("<br" :: String)) t
      recognized_update =
        if key == stat_update_text
        then Just $ scanStatusChange when user value -- ("STAT_UPDATE", value)
        else if key == owner_update_text
             then Just $ scanOwnerChange when user value
             else if key == label_update_text
                  then Just $ scanLabelChange when user value
                  else Nothing
  in recognized_update:(scanUpdateBox when user rest)

-- | Classify the update and put out any relevant IssueUpdates from it.
parseIssueUpdate tags =
  let no_comment = "(No comment was entered for this change.)"
      date_text = T.strip . innerText . fst . matchExternalTag ("<span class=date" :: String) $ tags
      parsed_date = TF.parseTime defaultTimeLocale "%b %e, %Y" (T.unpack date_text)
      when = maybe (UTCTime (ModifiedJulianDay 0) 0) id parsed_date
      user = innerText $ take 3 $ dropWhile (~/= ("<a class=userlink" :: String)) $ tags
      comment :: Text
      comment =
        T.pack $ normalizeInputText $ T.unpack $ T.strip
             $ innerText $ take 4 $ dropWhile (~/= ("<pre" :: String)) tags
      has_no_comment = no_comment == comment
      comment_result = if has_no_comment
                       then Nothing
                       else Just $ IssueEvent when user $ IssueComment comment
      update_box = fst $ matchExternalTag ("<div class=box-inner" :: String) tags
      updates = scanUpdateBox when user update_box
  in catMaybes (comment_result:(updates))

parseIssueText :: Text -> [IssueEvent]
parseIssueText text =
  let hist = extractHistoryTags $ parseTags text
      cl = separateChildrenDivs hist
  in concatMap parseIssueUpdate cl
