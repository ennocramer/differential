{-# LANGUAGE RecordWildCards #-}

-- | This module provides a UI for Differential based on Vty-UI.
module Differential.Ui ( runUI ) where

import           Brick
import qualified Brick.AttrMap        as BA
import qualified Brick.Focus          as BF
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.List   as BL

import           Data.Char            ( isControl, isSpace )
import           Data.Maybe           ( fromMaybe, listToMaybe )
import           Data.Text            ( Text )
import qualified Data.Text            as Text
import           Data.Vector          ( Vector )
import qualified Data.Vector          as Vector

import           Differential.Diff

import qualified Graphics.Vty         as Vty

data Name = NameFiles | NameDiff
    deriving (Eq, Ord, Show)

data State = State { stateFiles :: BL.List Name Diff
                   , stateDiff  :: BL.List Name (Text, BA.AttrName)
                   , stateFocus :: BF.FocusRing Name
                   }

attrHunk :: BA.AttrName
attrHunk = BA.attrName "hunk"

attrCtx :: BA.AttrName
attrCtx = BA.attrName "ctx"

attrAdd :: BA.AttrName
attrAdd = BA.attrName "add"

attrDel :: BA.AttrName
attrDel = BA.attrName "del"

maybeRenderDiff :: Maybe Diff -> Vector (Text, BA.AttrName)
maybeRenderDiff = maybe Vector.empty renderDiff

renderDiff :: Diff -> Vector (Text, BA.AttrName)
renderDiff diff = Vector.fromList $
    concatMap ($ diff)
              [ writeComment . diffComment
              , writeHeader . diffHeader
              , writeHunks . diffHunks
              ]
  where
    writeComment = map (write attrCtx)

    writeHeader header =
        map ($ header)
            [ write attrDel . headerOldLine, write attrAdd . headerNewLine ]

    writeHunks = concatMap writeHunk

    writeHunk hunk = write attrHunk (hunkLine hunk) :
        map writeLine (hunkLines hunk)

    writeLine (Line (t, s)) = write (lineColor t) s

    write clr chars = (protect chars, clr)

    protect = Text.concatMap protectChar
    protectChar c
        | c == '\n' = Text.empty
        | c == '\t' = Text.pack $ replicate 8 ' '
        | isSpace c = Text.singleton ' '
        | isControl c = Text.singleton '.'
        | otherwise = Text.singleton c

    lineColor New = attrAdd
    lineColor Old = attrDel
    lineColor Context = attrCtx

drawUI :: State -> [Widget Name]
drawUI State{..} = [ ui ]
  where
    files = vLimit 5 $
        BF.withFocusRing stateFocus (BL.renderList drawFiles) stateFiles
    diff = BF.withFocusRing stateFocus (BL.renderList drawDiff) stateDiff
    ui = vBox [ files, BB.hBorder, diff ]

    drawFiles _ d = padRight Max $ str $ diffTitle d
    drawDiff _ (line, attr) = withAttr attr $ padRight Max $ txt line

appEvent :: State -> Vty.Event -> EventM Name (Next State)
appEvent s e = do
    vp <- lookupViewport NameDiff
    let page = fromMaybe 1 (snd . _vpSize <$> vp)
    case e of
        Vty.EvKey (Vty.KChar '\t') [] -> continue $
            s { stateFocus = BF.focusNext (stateFocus s) }
        Vty.EvKey (Vty.KChar 'p') [] -> onFiles (return . BL.listMoveUp)
        Vty.EvKey (Vty.KChar 'n') [] -> onFiles (return . BL.listMoveDown)
        Vty.EvKey (Vty.KChar 'u') [] -> onDiff (return . BL.listMoveUp)
        Vty.EvKey (Vty.KChar 'd') [] -> onDiff (return . BL.listMoveDown)
        Vty.EvKey (Vty.KChar 'b') [] ->
            onDiff (return . BL.listMoveBy (negate page))
        Vty.EvKey (Vty.KChar ' ') [] -> onDiff (return . BL.listMoveBy page)
        Vty.EvKey (Vty.KChar 'q') [] -> halt s
        ev -> case BF.focusGetCurrent $ stateFocus s of
            Just NameFiles -> onFiles (BL.handleListEvent ev)
            Just NameDiff -> onDiff (BL.handleListEvent ev)
            Nothing -> continue s
  where
    onFiles :: (BL.List Name Diff -> EventM Name (BL.List Name Diff))
            -> EventM Name (Next State)
    onFiles f = do
        files <- f (stateFiles s)
        let render = renderDiff . snd <$> BL.listSelectedElement files
        let diff = BL.listReplace (fromMaybe Vector.empty render) (Just 0) $
                stateDiff s
        continue $ s { stateFiles = files, stateDiff = diff }

    onDiff :: (BL.List Name (Text, BA.AttrName)
               -> EventM Name (BL.List Name (Text, BA.AttrName)))
           -> EventM Name (Next State)
    onDiff f = do
        diff <- f (stateDiff s)
        continue $ s { stateDiff = diff }

appAttributes :: BA.AttrMap
appAttributes =
    BA.attrMap Vty.defAttr
               [ (BL.listSelectedAttr, Vty.white `on` Vty.brightBlack)
               , (BL.listSelectedFocusedAttr, Vty.white `on` Vty.blue)
               , (attrHunk, fg Vty.yellow)
               , (attrAdd, fg Vty.green)
               , (attrDel, fg Vty.red)
               ]

app :: App State Vty.Event Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const appAttributes
          , appLiftVtyEvent = id
          }

-- | Display a Patch using the Vty-UI UI.
runUI :: Patch -> IO ()
runUI (Patch diffs) = do
    _ <- defaultMain app initialState
    return ()
  where
    initialState =
        State { stateFiles = BL.list NameFiles (Vector.fromList diffs) 1
              , stateDiff = BL.list NameDiff
                                    (maybeRenderDiff $
                                         listToMaybe diffs)
                                    1
              , stateFocus = BF.focusRing [ NameFiles, NameDiff ]
              }