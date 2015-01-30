module Sync.Retrieve.GoogleCode.GoogleCode where

import qualified Data.Csv as CSV
import Data.Char (toLower, isSpace, ord, isAlphaNum)
import Data.Vector (Vector, toList)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Text.Encoding
import qualified Data.Map as M
import Sync.Issue.Issue
import Network.HTTP
import Network.URI (escapeURIString, isUnescapedInURI)

-- ID, Pri, Mstone, ReleaseBlock, Area, Status, Owner, Summary
type CSVRow = (Int, String, String, String, String, String, String, String, String)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


parseFile :: String -> Either String (Vector CSVRow)
parseFile file =
  let myOptions = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord ',') }
  in CSV.decodeWith myOptions CSV.HasHeader $ pack file

-- | Takes a project and a list of tags, returns [Issue]
fetch :: String -> [String] -> IO ([Issue])
fetch project tags = do
   let esctags = map (escapeURIString isUnescapedInURI) tags
       query = intercalate "%20" esctags
       uri = ("http://code.google.com/p/" ++ project ++ "/issues/csv?can=2&q=" ++
              query ++ "&colspec=ID%20Pri%20Mstone%20ReleaseBlock%20Area" ++
              "%20Status%20Owner%20Summary")
   body <- simpleHTTP (getRequest uri) >>= getResponseBody

   -- ID, Pri,	Mstone, ReleaseBlock,	Area,	Status, Owner, Summary, Labels
   let res = parseFile body
       lookup stat = M.lookup stat (M.fromList [
                                       ("assigned", Open), ("closed", Closed),
                                       ("open", Open)])
       xlate stat = maybe Open id $ lookup stat
       cleanChar c
        | isAlphaNum c = c
        | c == '-' = c
        | otherwise = '_'
       cleanTag tag = map cleanChar tag
       makeIssue :: CSVRow -> Issue
       makeIssue (id, prio, mstone, relblock, area, status, owner,
                  summary, labels) =
         Issue project id owner (xlate $ map toLower status) (
           map cleanTag $ map trim $ splitOn "," labels) summary "googlecode"
   case res of
     Left err -> do putStrLn $ "Failed parse from '" ++ uri ++ "': " ++ err
                    return []
     Right vals -> do let issues = toList vals :: [CSVRow]
                      return $ map makeIssue issues
