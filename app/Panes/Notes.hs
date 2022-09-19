{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.Notes
  (
    NotesPane
  )
where

import           Brick hiding ( Location )
import           Brick.Panes
import           Brick.Widgets.List
import           Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V

import           Defs

data NotesPane


instance Pane WName MyWorkEvent NotesPane (Maybe Location) where
  data (PaneState NotesPane MyWorkEvent) = N { nL :: List WName Note }
  type (InitConstraints NotesPane s) = ( HasLocation s
                                       , HasProjects s
                                       , HasSelection s
                                       )
  type (DrawConstraints NotesPane s WName) = ( HasFocus s WName
                                             , HasLocation s
                                             )
  initPaneState gs =
    let l = N (list (WName "Notes:List") mempty 1)
    in flip updatePane l $ snd <$> getCurrentLocation gs
  updatePane mbl ps =
    case mbl of
      Just l ->
        let ents = notes l
        in N $ listReplace (V.fromList ents) (Just 0) (nL ps)
      Nothing -> N $ listReplace mempty Nothing (nL ps)
  drawPane ps gs =
    let isFcsd = gs^.getFocus.to focused == Just (WName "Pane:Notes")
        rndr nt = str (show (notedOn nt) <> " -- ")
                  <+> txt (head $ T.lines $ note nt)
    in Just $ vBox [ renderList (const rndr) isFcsd (nL ps)
                     -- , hBorder
                   , vLimit 1 (fill '-')
                   , vLimitPercent 25
                     $ withVScrollBarHandles
                     $ withVScrollBars OnRight
                     $ viewport (WName "Notes:Scroll") Vertical
                     $ txtWrap
                     $ maybe "" (note . snd) $ listSelectedElement (nL ps)
                   ]
  focusable _ ps = focus1If (WName "Pane:Notes")
                   $ not $ null $ listElements $ nL ps
  handlePaneEvent _ ev = nList %%~ \w -> nestEventM' w (handleListEvent ev)


nList :: Lens' (PaneState NotesPane MyWorkEvent) (List WName Note)
nList f ps = (\n -> ps { nL = n }) <$> f (nL ps)
