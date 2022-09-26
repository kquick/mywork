{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Panes.Help
  ( HelpPane(..)
  , initHelp
  )
where

import           Brick
import           Brick.Panes
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import qualified Brick.Widgets.Core as BC
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import           Defs
import           Panes.Common.QQDefs


data HelpPane = HelpPane

instance Pane WName appEv HelpPane () where
  data (PaneState HelpPane appEv) = H Bool
  initPaneState _ = H False
  focusable _ (H ps) = focus1If (WName "Help") ps
  drawPane (H ps) _ = if ps then Just drawHelp else Nothing
  handlePaneEvent _ ev ps = handleHelpEvent ps ev


initHelp :: PaneState HelpPane appEv
initHelp = H True


drawHelp :: Widget WName
drawHelp =
  let hsize = 70
      infoPane = vLimitPercent 80 $ hLimitPercent hsize
                 $ borderWithLabel (txt "MyWork Help")
                 $ withVScrollBarHandles
                 $ withVScrollBars OnRight
                 $ viewport (WName "HelpScroll") Vertical
                 $ helpInfo
      helpInfo = vBox (txtWrap . (<> " ") <$> T.lines helpText)
      helpPane = padTop (BC.Pad 1) $ hLimitPercent hsize $ vBox
        [ hCenter $ txt "Arrow Up/Arrow Down : scroll vertically by lines"
        , hCenter $ txt "Page Up/Page Down : scroll vertically by page"
        , hCenter $ txt "ESC: quit"
        ]
  in centerLayer (infoPane <=> helpPane)


handleHelpEvent :: PaneState HelpPane appEv -> Vty.Event
                -> EventM WName es (PaneState HelpPane appEv)
handleHelpEvent hs = \case
  Vty.EvKey Vty.KEsc [] -> return $ H False
  Vty.EvKey Vty.KUp [] ->
    vScrollBy (viewportScroll (WName "HelpScroll")) (-1) >> return hs
  Vty.EvKey Vty.KDown [] ->
    vScrollBy (viewportScroll (WName "HelpScroll")) 1 >> return hs
  Vty.EvKey Vty.KPageUp [] ->
    vScrollBy (viewportScroll (WName "HelpScroll")) (-10) >> return hs
  Vty.EvKey Vty.KPageDown [] ->
    vScrollBy (viewportScroll (WName "HelpScroll")) 10 >> return hs
  _ -> return hs


helpText :: T.Text
helpText = [sq|
This is the displayed help information
----------------------------------------
The MyWork application is designed to help keep track of the things you are
working on.  This is particularly useful in a workflow where multiple projects
are being worked on, or where multiple instances of a particular project are
being worked on, with different efforts (bugfixes, features) in the different
instances.  Mentally switching contexts between the different projects and
locations can be a challenge, especially when trying to remember what was being
done in a particular location, or the relationship of that location to other
locations.

The MyWork application is a simple application that organizes and remembers
these different projects and locations, along with any notes you'd like to
attach to each location.  When switching from working on one thing to the next,
this application can provide a place to make a note about what was in-progress
in the current location, and read about the status of the new location to
reconstruct the context for that location.

Locations can be local directories or network VCS locations (e.g. github).
Local locations can provide more information, but it is helpful to also track
the public or remote locations for projects.

The display is divided into the following primary parts:

    +--------------------------------+
    | Summary                        |
    |--------------------------------|
    | Projects | Locations           |
    |          |                     |
    |          |                     |
    |          |                     |
    |          |---------------------|
    |          | Location Notes      |
    |          |                     |
    |          |                     |
    |--------------------------------|
    |           Controls             |
    +--------------------------------+


   TAB / Shift+TAB              -- switch between active panes


----------------------------------------
|]
