{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.Projects () where

import           Brick
import           Brick.Panes
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Control.Lens
import qualified Data.Sequence as Seq
import           Data.Text ( Text )
import qualified Data.Text.Zipper as TZ
import qualified Data.Vector as V

import           Defs


instance Pane WName MyWorkEvent Projects Projects where
  data (PaneState Projects MyWorkEvent) = P { pL :: List WName (Role, Text)
                                            , pS :: Editor Text WName
                                            }
  type (InitConstraints Projects s) = ( HasProjects s )
  type (DrawConstraints Projects s WName) = ( HasFocus s WName )
  type (EventType Projects WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  initPaneState s =
    let prjs = projects $ snd $ getProjects s
        pl = list (WName "Projs:List") (V.fromList (mkListEnt <$> prjs)) 1
        ps = editor (WName "Projs:Filter") (Just 1) ""
    in P pl ps
  drawPane ps gs =
    let isFcsd = gs^.getFocus.to focused == Just (WName "Pane:ProjList")
        renderListEnt _ (r,n) = withAttr (roleAttr r) $ txt n
        lst = renderList renderListEnt isFcsd (pL ps)
        srch = str "Search: " <+> renderEditor (txt . head) isFcsd (pS ps)
    in Just $ vBox [ lst, fill ' ', srch ]
  handlePaneEvent _ ev ps =
    do ps1 <- case ev of
                VtyEvent ev' ->
                  ps & pList %%~ \w -> nestEventM' w (handleListEvent ev')
                _ -> return ps
       ps2 <- ps1 & pSrch %%~ \w -> nestEventM' w (handleEditorEvent ev)
       return ps2
  focusable _ _ = Seq.singleton (WName "Pane:ProjList")
  updatePane newprjs =
    (pList %~ listReplace (V.fromList (mkListEnt <$> projects newprjs)) (Just 0))
    .
    (pSrch . editContentsL %~ TZ.clearZipper)


pList :: Lens' (PaneState Projects MyWorkEvent) (List WName (Role, Text))
pList f ps = (\n -> ps { pL = n }) <$> f (pL ps)

pSrch :: Lens' (PaneState Projects MyWorkEvent) (Editor Text WName)
pSrch f ps = (\n -> ps { pS = n }) <$> f (pS ps)

mkListEnt :: Project -> (Role, Text)
mkListEnt pr = (role pr, name pr)


instance HasSelection (PaneState Projects MyWorkEvent) where
  selectedProject = fmap snd . fmap snd . listSelectedElement . pL
