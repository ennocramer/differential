-- |This module provides a UI for Differential based on Vty-UI.
module Differential.Ui (runUI) where

import Graphics.Vty
import Graphics.Vty.Widgets.All

import Differential.Diff
import System.Exit (exitSuccess)
import qualified Data.Text as T
import qualified Data.Char as C

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
renderDiff diff = concatMap ($ diff) [ writeComment . diffComment
                                     , writeHeader . diffHeader
                                     , writeHunks . diffHunks ]
  where
    writeComment = map (write ctxColor)

    writeHeader header = map ($ header) [ write delColor . headerOldLine
                                        , write addColor . headerNewLine ]

    writeHunks = concatMap writeHunk

    writeHunk hunk = (write hunkColor $ hunkLine $ hunk) :
                     (map writeLine $ hunkLines $ hunk)

    writeLine (Line (t, s)) = write (lineColor t) s

    write clr txt = (protect txt, fgColor clr)

    protect = T.concatMap protectChar
    protectChar c =
      if c == '\n' then T.empty
      else if c == '\t' then T.pack $ replicate 8 ' '
      else if C.isSpace c then T.singleton ' '
      else if C.isControl c then T.singleton '.'
      else T.singleton c

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
