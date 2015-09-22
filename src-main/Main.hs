-- | This module defines the main function and command line handling
-- for the Differential utility program.

module Main (main) where

import Differential.Parser
import Differential.Ui
import System.Environment

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as IO

-- | Parse and display a patch from standard input or file.
main :: IO ()
main = do
  args <- getArgs
  case args of
    f : _ -> run (IO.readFile f)
    _     -> run IO.getContents

-- | Parse and display patch from any IO Text.
run :: IO Text -> IO ()
run source = do
  input <- source
  case parse input of
    Left err -> putStrLn err
    Right patch -> runUI patch
