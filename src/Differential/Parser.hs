{-# LANGUAGE OverloadedStrings #-}

-- |  A Parsec-based parser for unified diffs.
module Differential.Parser ( Differential.Parser.parse ) where

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as L

import           Differential.Diff

import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.Text.Lazy

-- | Determine the type of diff based on the two recorded file names.
diffType :: String -> String -> ChangeType
diffType old new
    | old' == new' = FileModified
    | old' == "/dev/null" = FileAdded
    | new' == "/dev/null" = FileDeleted
    | otherwise = FileRenamed
  where
    old' = realPath old
    new' = realPath new

-- | Determine the type of diff based on 'diff --git' output
gitDiffType :: T.Text -> ChangeType
gitDiffType str
    | "new " `T.isPrefixOf` str = FileAdded
    | "deleted " `T.isPrefixOf` str = FileDeleted
    | "rename " `T.isPrefixOf` str = FileRenamed
    | otherwise = FileModified

-- | Return whether a hunk line is Old, New, or Context line.
lineType :: String -> LineType
lineType ('+' : _) = New
lineType ('-' : _) = Old
lineType _ = Context

-- | Parse a unified diff, returning either a Patch object or a parser
-- error message.
parse :: L.Text -> Either String Patch
parse input = case Text.Parsec.Prim.parse patch "" input of
    Left e -> Left $ show e
    Right p -> Right p

patch :: Parser Patch
patch = do
    diffs <- many (gitDiff <|> diff)
    eof
    return $ Patch diffs

diff :: Parser Diff
diff = do
    cmt <- comment
    hdr <- header
    hnk <- many hunk
    _ <- optional $ string "\\ No newline at end of file" <* endOfLine
    return Diff { diffComment = cmt, diffHeader = hdr, diffHunks = hnk }

gitDiff :: Parser Diff
gitDiff = do
    _ <- lookAhead $ string "diff --git "
    diffline <- lookAhead text
    _ <- string "diff --git "
    oldFile <- many1 $ noneOf " \t\r\n"
    _ <- skipMany1 space
    newFile <- many1 $ noneOf "\t\r\n"
    _ <- endOfLine
    gitline <- lookAhead text
    cmt <- comment
    hdr <- try header <|>
               return Header { headerType = gitDiffType gitline
                             , headerOldFile = oldFile
                             , headerNewFile = newFile
                             , headerOldLine = T.pack $ "--- " ++ oldFile
                             , headerNewLine = T.pack $ "+++ " ++ newFile
                             }
    hnk <- many hunk
    return Diff { diffComment = diffline : cmt
                , diffHeader = hdr
                , diffHunks = hnk
                }

comment :: Parser [T.Text]
comment = many $ notFollowedBy notAComment >> text
  where
    notAComment = try (string "--- ") <|> try (string "diff --git ")

header :: Parser Header
header = do
    oldLine <- lookAhead text
    _ <- string "--- "
    oldFile <- many1 $ noneOf "\t\r\n"
    _ <- text
    newLine <- lookAhead text
    _ <- string "+++ "
    newFile <- many1 $ noneOf "\t\r\n"
    _ <- text
    return Header { headerType = diffType oldFile newFile
                  , headerOldFile = oldFile
                  , headerNewFile = newFile
                  , headerOldLine = oldLine
                  , headerNewLine = newLine
                  }

hunk :: Parser Hunk
hunk = do
    full <- lookAhead text
    _ <- string "@@ "
    _ <- char '-'
    old <- range
    _ <- skipMany1 space
    _ <- char '+'
    new <- range
    _ <- string " @@"
    _ <- many $ oneOf " \t"
    cmt <- text
    lns <- many line
    return Hunk { hunkOldRange = old
                , hunkNewRange = new
                , hunkComment = cmt
                , hunkLine = full
                , hunkLines = lns
                }

range :: Parser (Int, Int)
range = do
    from <- many1 digit
    _ <- char ','
    to <- many1 digit
    return (read from, read to)

line :: Parser Line
line = do
    try $ notFollowedBy header
    _ <- lookAhead $ oneOf "+- \\"
    content <- text
    return $ Line (lineType (T.unpack content), content)

text :: Parser T.Text
text = do
    content <- many (noneOf "\r\n")
    _ <- endOfLine
    return $ T.pack content
