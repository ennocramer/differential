-- |This module provides the Differential main function.
module Main (main) where

import Differential.Parser
import System.Environment

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as IO

-- |Parse a Patch from standard input and display it using the configured UI.
main :: IO ()
main = do
  args <- getArgs
  case args of
    f : _ -> run (IO.readFile f)
    _     -> run IO.getContents

run :: IO Text -> IO ()
run source = do
  input <- source
  case parse input of
    Left err -> putStrLn err
    Right patch -> putStrLn $ show patch
