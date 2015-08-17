-- |A Parsec-based parser for unified diffs.
module Differential.Parser (
  Differential.Parser.parse
  ) where

import Differential.Diff
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text.Lazy
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

-- |Determine the type of diff based on the two recorded file names.
diffType :: String -> String -> ChangeType
diffType old new
  | old' == new' = FileModified
  | old' == "/dev/null" = FileAdded
  | new' == "/dev/null" = FileDeleted
  | otherwise = FileRenamed
  where
    old' = realPath old
    new' = realPath new

-- |Return whether a hunk line is Old, New, or Context
-- line.
lineType :: String -> LineType
lineType ('+' : _) = New
lineType ('-' : _) = Old
lineType _ = Context

-- |Parse a unified diff, returning either a Patch object or a parser
-- error message.
parse :: L.Text -> Either String Patch
parse input =
  case Text.Parsec.Prim.parse patch "" input of
    Left e -> Left $ show e
    Right p -> Right p

patch :: Parser Patch
patch = do
  diffs <- many diff
  eof
  return $ Patch diffs

diff :: Parser Diff
diff = do
  cmt <- comment
  hdr <- header
  hnk <- many hunk
  return Diff { diffComment = map T.pack cmt
              , diffHeader = hdr
              , diffHunks = hnk }

comment :: Parser [String]
comment = many $ try (notFollowedBy header) >> text

header :: Parser Header
header = do
  oldLine <- lookAhead text
  _       <- string "--- "
  oldFile <- many1 $ noneOf "\t\r\n"
  _       <- text
  newLine <- lookAhead text
  _       <- string "+++ "
  newFile <- many1 $ noneOf "\t\r\n"
  _       <- text
  return Header { headerType = diffType oldFile newFile
                , headerOldFile = oldFile
                , headerNewFile = newFile
                , headerOldLine = T.pack oldLine
                , headerNewLine = T.pack newLine }

hunk :: Parser Hunk
hunk = do
  full <- lookAhead text
  _    <- string "@@ "
  _    <- char '-'
  old  <- range
  _    <- skipMany1 space
  _    <- char '+'
  new  <- range
  _    <- string " @@"
  _    <- many $ oneOf " \t"
  cmt  <- text
  lns  <- many line
  return Hunk { hunkOldRange = old
              , hunkNewRange = new
              , hunkComment  = T.pack cmt
              , hunkLine     = T.pack full
              , hunkLines    = lns }

range :: Parser (Int, Int)
range = do
  from <- many1 digit
  _    <- char ','
  to   <- many1 digit
  return (read from, read to)

line :: Parser Line
line = do
  try $ notFollowedBy header
  _       <- lookAhead $ oneOf "+- "
  content <- text
  return $ Line (lineType content, T.pack content)

text :: Parser String
text = do
  content <- manyTill anyChar (try (string "\r\n") <|> string "\n")
  return $ content
