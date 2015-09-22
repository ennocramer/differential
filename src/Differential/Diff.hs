-- | This module defines types to represent unified diffs and
-- associated convenience functions.

module Differential.Diff
    ( Patch(..)
    , Diff(..)
    , Header(..)
    , ChangeType(..)
    , Hunk(..)
    , Line(..)
    , LineType(..)
    , diffTitle
    , realPath
    ) where

import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
import Control.Applicative ((<|>))

-- | A Patch consists of a series of file Diffs.
data Patch = Patch [Diff]
  deriving (Eq, Show)

-- | A Diff consists of a Header and a series of Hunks, optionally
-- preceeded by comment lines.
data Diff = Diff { diffComment :: [T.Text]
                 , diffHeader  :: Header
                 , diffHunks   :: [Hunk]
                 }
  deriving (Eq, Show)

-- | A Diff Header consists of the Diff Type, the old and new file
-- names, and the unprocessed header lines.
data Header = Header { headerType    :: ChangeType
                     , headerOldFile :: FilePath
                     , headerNewFile :: FilePath
                     , headerOldLine :: T.Text
                     , headerNewLine :: T.Text
                     }
  deriving (Eq, Show)

-- | Indicates whether a file was added, removed, renamed or simply modified.
data ChangeType = FileAdded
                | FileDeleted
                | FileRenamed
                | FileModified
  deriving (Eq, Show)

-- | A Hunk consists of the old and new line ranges affected by the
-- hunk, an optional comment following the range declaration, the
-- unprocessed hunk header line, and a series of change Lines.
data Hunk = Hunk { hunkOldRange :: (Int, Int)
                 , hunkNewRange :: (Int, Int)
                 , hunkComment  :: T.Text
                 , hunkLine     :: T.Text
                 , hunkLines    :: [Line]
                 }
  deriving (Eq, Show)

-- | A Line consists of a type and its content.
data Line = Line (LineType, T.Text)
  deriving (Eq, Show)

-- | Indicates whether a line is an addition, a removal, or simply context.
data LineType = Context
              | Old
              | New
  deriving (Eq, Show)

-- | Generate a simple title for a given Diff.
diffTitle :: Diff -> String
diffTitle diff =
  case headerType header of
    FileAdded    -> "A " ++ new
    FileDeleted  -> "R " ++ old
    FileRenamed  -> "M " ++ new ++ " (was " ++ old ++ ")"
    FileModified -> "M " ++ new
  where
    header = diffHeader diff
    old = realPath $ headerOldFile header
    new = realPath $ headerNewFile header

-- | Strip any elements from a path that were added by the diff
-- generator.  This function currently strips "a\/" and "b\/" prefixes.
realPath :: FilePath -> FilePath
realPath p = fromMaybe p $ stripPrefix "a/" p <|> stripPrefix "b/" p
