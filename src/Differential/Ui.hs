-- |This module provides a UI for Differential based on Vty-UI.
module Differential.Ui (runUI) where

import Graphics.Vty
import Graphics.Vty.Widgets.All

import Differential.Diff
import Control.Monad.Writer (runWriter, tell)
import System.Exit (exitSuccess)
import qualified Data.Text as T

ctxColor :: Color
ctxColor = white

addColor :: Color
addColor = green

delColor :: Color
delColor = red

hunkColor :: Color
hunkColor = yellow

lineColor :: LineType -> Color
lineColor New = addColor
lineColor Old = delColor
lineColor Context = ctxColor

renderDiff :: Diff -> [(T.Text, Attr)]
renderDiff diff = snd $ runWriter $ do
  writeComment $ diffComment diff
  writeHeader $ diffHeader diff
  writeHunks $ diffHunks diff
  where
    writeComment = mapM_ (write ctxColor)

    writeHeader header = do
      write delColor $ headerOldLine header
      write addColor $ headerNewLine header

    writeHunks = mapM_ writeHunk

    writeHunk hunk = do
      write hunkColor $ hunkLine hunk
      mapM_ writeLine $ hunkLines hunk

    writeLine (Line (t, s)) = write (lineColor t) s

    write clr txt = tell [(txt, fgColor clr)]

-- |Display a Patch using the Vty-UI UI.
runUI :: Patch -> IO ()
runUI (Patch diffs) = do
  files <- newList 1
  patch <- newList 1
  ui <- vFixed 5 files <--> hBorder <--> return patch

  fg <- newFocusGroup
  _ <- addToFocusGroup fg files
  _ <- addToFocusGroup fg patch

  c <- newCollection
  _ <- addToCollection c ui fg

  fg `onKeyPressed` \_ key _ ->
    case key of
      KChar 'p' -> do scrollUp files; return True
      KChar 'n' -> do scrollDown files; return True
      KChar 'u' -> do scrollUp patch; return True
      KChar 'd' -> do scrollDown patch; return True
      KChar 'b' -> do pageUp patch; return True
      KChar ' ' -> do pageDown patch; return True
      KChar 'q' -> exitSuccess
      _         -> return False

  files `onSelectionChange` \ev -> do
    clearList patch
    case ev of
      SelectionOn _ diff _ -> mapM_ (\line -> addToList patch () =<< plainTextWithAttrs [line]) (renderDiff diff)
      SelectionOff -> return ()

  mapM_ (\d -> addToList files d =<< plainText (T.pack $ diffTitle d)) diffs
  setSelected files 0

  runUi c defaultContext
