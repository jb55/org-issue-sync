{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Data.Issue where
import Data.Ord ()
import Data.Hashable
import Data.Bits
import Data.Time
import Data.Text (Text)

data IssueStatus = Open | Active | Closed deriving (Eq, Show)

data IssueEventDetails = IssueStatusChange { isNewStatus :: IssueStatus }
                       | IssueComment { icComment :: Text }
                       | IssueOwnerChange { ieNewOwner :: Text }
                       | IssueLabelChange { ilNewLabels :: [Text],
                                            ilRemovedLabels :: [Text] }
                       | IssueMilestoneChange { imNewMileStone :: Maybe Text,
                                                imOldMileStone :: Maybe Text }
                       deriving (Eq, Show)

data IssueEvent = IssueEvent
                  { ieWhen :: UTCTime
                  , ieUser :: Text
                  , ieDetails :: IssueEventDetails
                  } deriving (Eq, Show)

data Issue = Issue
             { origin :: Text
             , number :: Int
             , user :: Text
             , status :: IssueStatus
             , tags :: [Text]
             , summary :: Text
             , iType :: Text
             , iUrl :: Text
             , events :: [IssueEvent]
             } deriving (Show)

issueEqual :: Issue -> Issue -> Bool
issueEqual l r =
  (origin l == origin r) && (number l == number r) && (iType l == iType r)

instance Eq Issue where
  (==) = issueEqual

-- |Try to make this as fast as we can.
instance Ord Issue where
  compare l r =
    let !l_num = number l
        !r_num = number r
        !num_comp = compare l_num r_num
        l_type = iType l
        r_type = iType r
        type_comp = compare l_type r_type
        l_origin = origin l
        r_origin = origin r
        origin_comp = compare l_origin r_origin
    in if num_comp == EQ
       then if type_comp == EQ
            then origin_comp
            else type_comp
       else num_comp

instance Ord IssueEvent where
  compare l r = compare (ieWhen l) (ieWhen r)

instance Hashable Issue where
  hashWithSalt salt iss =
    (hashWithSalt salt $ origin iss) `xor` (
      hashWithSalt salt $ number iss) `xor` (
      hashWithSalt salt $ iType iss)
