{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.Notes () where

import           Brick hiding ( Location )
import           Brick.Panes
import           Brick.Widgets.List
import           Control.Lens
import           Control.Monad ( join )
import qualified Data.List as DL
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty

import           Defs


instance Pane WName MyWorkEvent Note where
  data (PaneState Note MyWorkEvent) = N { nL :: List WName Note
                                        , nLoc :: Maybe Location
                                        }
  type (InitConstraints Note s) = ( HasLocation s
                                  , HasProjects s
                                  , HasSelection s
                                  )
  type (DrawConstraints Note s WName) = ( HasFocus s WName
                                        , HasLocation s
                                        , HasDate s
                                        )
  type (UpdateType Note) = Maybe Location
  initPaneState gs =
    let l = N (list (WName "Notes:List") mempty 1) Nothing
    in flip updatePane l $ join $ snd <$> getCurrentLocation gs
  updatePane mbl ps =
    let curElem = maybe id listMoveTo $ listSelected $ nL ps
        nl =
          case mbl of
            Just l -> curElem
                      . listReplace (V.fromList $ sortNotes $ l^.notesL) (Just 0)
            Nothing -> listReplace mempty Nothing
        sortNotes = DL.reverse . DL.sort -- most recent first
    in N (nl (nL ps)) mbl
  drawPane ps gs =
    let isFcsd = gs^.getFocus.to focused == Just WNotes
        rndr nt = ttlAttr nt
                  $ str (show (nt ^. notedOnL))
                  <+> txt " -"
                  <+> (if nt ^. noteSourceL == ProjLoc
                      then txt "@ "
                      else txt "- ")
                  <+> ttl nt
        ttlAttr nt = case nt ^. noteSourceL of
                       MyWorkDB -> withAttr a'NoteSourceMyWork
                       ProjLoc -> withAttr a'NoteSourceProjLoc
                       MyWorkGenerated -> withAttr a'NoteSourceGenerated
        ttl n = case noteKeyword n of
                  (Just (FUTURE_ d), NoteRemTitle r) ->
                    withAttr a'NoteWordFuture (txt "FUTURE")
                    <+> txt " "
                    <+> (if getToday gs <= d
                          then withAttr a'NoteWordPending $ str $ show d
                          else withAttr a'NoteWordExpired $ str $ show d)
                    <+> txt " "
                    <+> txt r
                  (Just (TODO_ d), NoteRemTitle r) ->
                    withAttr a'NoteWordTODO (txt "TODO")
                    <+> txt " "
                    <+> (if getToday gs <= d
                          then withAttr a'NoteWordPending $ str $ show d
                          else withAttr a'NoteWordExpired $ str $ show d)
                    <+> txt " "
                    <+> txt r
                  (Just o, NoteRemTitle r) ->
                    withAttr (noteKeywordAttr o) (str $ show o)
                    <+> txt " "
                    <+> txt r
                  (Nothing, NoteRemTitle t) -> txt t
    in Just $ vBox [ withVScrollBars OnRight
                     $ renderList (const rndr) isFcsd (nL ps)
                     -- , hBorder
                   , vLimit 1 (fill '-'
                               <+> str " vv - Full Note - vv "
                               <+> fill '-')
                   , let rndrN nt = vLimit 1 (ttlAttr nt $ ttl nt)
                                    <=>
                                    (vLimitPercent 75
                                     $ withVScrollBars OnRight
                                     $ viewport (WName "Notes:Scroll") Vertical
                                     $ txtWrap
                                     $ noteBody
                                     $ nt)
                         blank = str " "
                     in maybe blank (rndrN . snd) $ listSelectedElement $ nL ps
                   ]
  focusable _ ps = focus1If WNotes $ not $ null $ listElements $ nL ps
  handlePaneEvent _ =
    let scroll o amt = \ps ->
          do _ <- o (viewportScroll (WName "Notes:Scroll")) amt
             return ps
    in \case
      --   * Scroll full note region with CTRL-up/down/page-up/page-down
      Vty.EvKey Vty.KUp [Vty.MCtrl] -> scroll vScrollBy (-1)
      Vty.EvKey Vty.KDown [Vty.MCtrl] -> scroll vScrollBy 1
      Vty.EvKey Vty.KLeft [Vty.MCtrl] -> scroll hScrollBy (-1)
      Vty.EvKey Vty.KRight [Vty.MCtrl] -> scroll hScrollBy 1
      Vty.EvKey Vty.KPageUp [Vty.MCtrl] -> scroll vScrollBy (-10)
      Vty.EvKey Vty.KPageDown [Vty.MCtrl] -> scroll vScrollBy 10
      ev -> nList %%~ \w -> nestEventM' w (handleListEvent ev)


nList :: Lens' (PaneState Note MyWorkEvent) (List WName Note)
nList f ps = (\n -> ps { nL = n }) <$> f (nL ps)


instance HasNote (PaneState Note MyWorkEvent) where
  selectedNote ps = do
    curr <- listSelectedElement $ nL ps
    locn <- nLoc ps
    return ( locn ^. locationL, noteTitle $ snd curr )
