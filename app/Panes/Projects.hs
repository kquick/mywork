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
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Data.Vector as V

import           Defs


instance Pane WName MyWorkEvent Projects Projects where
  data (PaneState Projects MyWorkEvent) = P { pL :: List WName (Role, Text)
                                            , pS :: Editor Text WName
                                            , oC :: [ (Role, Text) ]
                                            }
  type (InitConstraints Projects s) = ( HasProjects s )
  type (DrawConstraints Projects s WName) = ( HasFocus s WName )
  type (EventType Projects WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  initPaneState s =
    let prjs = projects $ snd $ getProjects s
        oc = mkListEnt <$> prjs
        pl = list (WName "Projs:List") (V.fromList oc) 1
        ps = editor (WName "Projs:Filter") (Just 1) ""
    in P pl ps oc
  drawPane ps gs =
    let isFcsd = gs^.getFocus.to focused == Just WProjList
        renderListEnt _ (r,n) = withAttr (roleAttr r) $ txt n
        lst = renderList renderListEnt isFcsd (pL ps)
        srch = str "Search: " <+> renderEditor (txt . head) isFcsd (pS ps)
    in Just $ vBox [ lst, fill ' ', srch ]
  handlePaneEvent _ ev ps =
    do ps1 <- case ev of
                VtyEvent ev' ->
                  ps & pList %%~ \w -> nestEventM' w (handleListEvent ev')
                _ -> return ps
       let origSearchText = head $ ps ^. pSrch . to getEditContents
       ps2 <- ps1 & pSrch %%~ \w -> nestEventM' w (handleEditorEvent ev)
       let searchText = head $ ps2 ^. pSrch . to getEditContents
       if searchText == origSearchText
         then return ps2
         else let nc = if T.null searchText
                       then oC ps2
                       else filter ((searchText `T.isInfixOf`) . snd) (oC ps2)
                  np = if null nc then Nothing else Just 0
              in return $ ps2 & pList %~ listReplace (V.fromList nc) np
  focusable _ _ = Seq.singleton WProjList
  updatePane newprjs =
    let oc = mkListEnt <$> projects newprjs
    in (pList %~ listReplace (V.fromList oc) (Just 0))
       .
       (pSrch . editContentsL %~ TZ.clearZipper)
       .
       (pOrig .~ oc)


pList :: Lens' (PaneState Projects MyWorkEvent) (List WName (Role, Text))
pList f ps = (\n -> ps { pL = n }) <$> f (pL ps)

pSrch :: Lens' (PaneState Projects MyWorkEvent) (Editor Text WName)
pSrch f ps = (\n -> ps { pS = n }) <$> f (pS ps)

pOrig :: Lens' (PaneState Projects MyWorkEvent) [ (Role, Text) ]
pOrig f ps = (\n -> ps { oC = n }) <$> f (oC ps)

mkListEnt :: Project -> (Role, Text)
mkListEnt pr = (role pr, name pr)


instance HasSelection (PaneState Projects MyWorkEvent) where
  selectedProject = fmap snd . fmap snd . listSelectedElement . pL
