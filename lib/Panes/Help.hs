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
import           Brick.Widgets.Center
import qualified Brick.Widgets.Core as BC
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import           Defs
import           Panes.Common.Attrs
import           Panes.Common.QQDefs


data HelpPane = HelpPane

instance Pane WName appEv HelpPane where
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
                 $ modalB "MyWork Help"
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


   TAB / Shift+TAB     -- switch between active panes
   Ctrl-S              -- save (to previously loaded file, if any)
   Ctrl-Q              -- quit

The full note area can be scrolled by using the Ctrl key in conjunction with
the arrow and page-up/page-down keys.

Each Project can have zero or more locations where that Project exists.  These
locations can be local directories, remote URLs (e.g. github), or any other
desired text.  Any local directory location may be scanned to find additional
locations automatically.  Each location can optionally have a date, which is
usually the date of the last update (local directory locations will
automatically update the date with the last modified date of the top-level
directory); the date is user information and locations will be sorted primarily
by date and secondarily by location text.

Each Location is associated with zero or more Notes.  Each Note is a free-form
text paragraph, whose first line acts as the note "title".  In addition to notes
entered by the user in mywork, if the location is local and contains an
"@MyWork" directory, any file in that directory with the .txt extension will be
added as a note.  The first word of a note may be one of the following words
which will be highlighted specially:

  * TODO [date]
  * IN-PROGRESS
  * FUTURE [date]
  * BLOCKING

The TODO and FUTURE words may be followed by another word that specifies a date.
Dates in the past (expired) will be highlighted differently than dates in the
future (pending).

The Summary area shows the total number of projects (by role), as well as the
date range of locations.

----------------------------------------
|]
