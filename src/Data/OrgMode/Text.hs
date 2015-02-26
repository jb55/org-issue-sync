{-# LANGUAGE BangPatterns #-}
module Data.OrgMode.Text where

import Data.Monoid
import Data.Char (isSpace, toUpper, isPrint)

-- | Line numbers, where we can have an unattached root.
data LineNumber = NoLine
                | Line Int
                deriving (Eq, Show)

instance Monoid LineNumber where
  mempty = NoLine
  mappend NoLine _ = NoLine
  mappend _ NoLine = NoLine
  mappend (Line a) (Line b) = Line (a+b)

toNumber :: Int -> LineNumber -> Int
toNumber n NoLine = n
toNumber _ (Line a) = a

instance Num LineNumber where
  (+) a b = mconcat [a, b]
  (*) NoLine a = a
  (*) a NoLine = a
  (*) (Line a) (Line b) = Line $ a * b
  (-) a NoLine = a
  (-) NoLine b = NoLine
  (-) (Line a) (Line b) = Line $ a - b
  negate NoLine = NoLine
  negate (Line l) = Line (-l)
  abs NoLine = NoLine
  abs (Line l) = Line $ abs l
  signum NoLine = NoLine
  signum (Line l) 
    | l < 0 = Line (-1)
    | l > 0 = Line 1
    | otherwise = Line 0
  fromInteger i = Line $ fromIntegral i

instance Ord LineNumber where
  compare NoLine NoLine = EQ
  compare NoLine (Line _) = LT
  compare (Line _) NoLine = GT
  compare (Line a) (Line b) = compare a b

isNumber :: LineNumber -> Bool
isNumber NoLine = False
isNumber (Line _) = True

linesStartingFrom :: LineNumber -> [LineNumber]
linesStartingFrom NoLine = repeat NoLine
linesStartingFrom (Line l) = map Line [l..]

-- | Raw data about each line of text.  Currently a bit hacked, with
-- 'tlLineNum == 0' indicating a fake line.
data TextLine = TextLine
                { tlIndent :: Int  -- how long of a whitespace prefix is in tlText?
                , tlText :: String
                , tlLineNum :: LineNumber
                } deriving (Eq)



hasNumber :: TextLine -> Bool
hasNumber (TextLine _ _ (Line _)) = True
hasNumber _ = False

instance Show TextLine where
  show tl = (show $tlLineNum tl) ++ ":" ++ (tlText tl)

instance Ord TextLine where
  compare a b = compare (tlLineNum a) (tlLineNum b)



-- | Currently a simple getter.  TODO(lally): extend with enough here
-- to let us write out a modified .org file, preserving as much of the
-- original input document structure as we can.
class TextLineSource s where
  getTextLines :: s -> [TextLine]

-- | Normalizes out newlines to UNIX format.  CR -> LF, CRLF -> LF
normalizeInputText :: String -> String
normalizeInputText text =
  -- Operators that work on reversed strings.
  let swapCrLf :: String -> Char -> String
      swapCrLf ('\r':cs) '\n' = '\n':cs
      swapCrLf ('\n':cs) '\r' = '\n':cs
      swapCrLf cs c = c:cs
      -- A good place for fixing unprintable chars, but we have to
      -- identify them.
      swapCr :: String -> Char -> String
      swapCr cs '\r' = '\n':cs
      swapCr cs c = c:cs
      revStr = reverse text
      swappedCrLf = foldl swapCrLf "" revStr
      swappedCr = foldl swapCr "" swappedCrLf
  in reverse swappedCr

trimEndOfLine :: String -> String
trimEndOfLine f = reverse $ dropWhile isSpace $ reverse f

wrapString :: Int -> String -> String
wrapString _ [] = []
wrapString len str
  | length str < len = str
  | otherwise =
    let first_word = takeWhile (not . isSpace) str
        is_first_too_long = length first_word >= len
        max_width = reverse $ take len str
        wrapped_back = if is_first_too_long
                       then first_word
                       else reverse $ dropWhile (not . isSpace) max_width
        remain = drop (length wrapped_back) str
    in if length wrapped_back > 0 || length remain > 0
       then wrapped_back ++ "\n" ++ wrapString len remain
       else ""

wrapLine :: Int -> TextLine -> [TextLine]
wrapLine width (TextLine indent string linenum) =
  let desired_len = width - indent
      strings = concatMap lines $ map (wrapString desired_len) $ lines string
      line_nrs = linesStartingFrom linenum
      makeTextLine (str, nr) = TextLine indent (trimEndOfLine str) nr
  in map makeTextLine $ zip strings line_nrs

prefixLine :: String -> TextLine -> TextLine
prefixLine pfx (TextLine indent string linenum) =
  let new_str = pfx ++ string
      new_indent = length $ takeWhile isSpace new_str
  in TextLine new_indent new_str linenum

makeDrawerLines :: LineNumber -> Int -> String -> [(String, String)] -> [TextLine]
makeDrawerLines fstLine depth name props =
  let !indent = take depth $ repeat ' '
      headline =
        TextLine depth (indent ++ ":" ++ (map toUpper name) ++ ":") fstLine
      mAdd  (Just x) y = Just (x + y)
      mAdd Nothing y = Nothing
      lastline =
        TextLine depth (indent ++ ":END:") (mappend fstLine $ Line (length props + 1))
      makePropLine ((prop, value), nr) =
        TextLine depth (indent ++ ":" ++ prop ++ ": " ++ value) (mappend fstLine $ Line nr)
      proplines = map makePropLine $ zip props [1..]
  in (headline:(proplines)) ++ [lastline]

