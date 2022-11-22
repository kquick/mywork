{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.List as DL
import           Data.Maybe
import qualified Data.Sequence as Seq
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import           Data.Time.Calendar
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import           Numeric.Natural

import           Defs


data ListEnt = LE { leRole :: Role
                  , lePName :: ProjectName
                  , leInProg :: Bool
                  , lePending :: Natural
                  , lePendDay :: Maybe Day
                  , leFuture :: Bool -- non-date FUTURE
                  , leBlocking :: Bool
                  }
  deriving (Eq, Ord)


instance Pane WName MyWorkEvent Projects where
  data (PaneState Projects MyWorkEvent) = P { pL :: List WName ListEnt
                                            , pS :: Editor Text WName
                                            , oC :: [ ListEnt ]
                                            }
  type (InitConstraints Projects s) = ( HasProjects s )
  type (DrawConstraints Projects s WName) = ( HasFocus s WName, HasDate s )
  type (EventType Projects WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  type (UpdateType Projects) = Projects

  initPaneState s =
    let prjs = projects $ snd $ getProjects s
        oc = DL.sort (mkListEnt <$> prjs)
        pl = list (WName "Projs:List") (V.fromList oc) 1
        ps = editor (WName "Projs:Filter") (Just 1) ""
    in P pl ps oc

  drawPane ps gs =
    let isFcsd = gs^.getFocus.to focused == Just WProjList
        renderListEnt _ le =
          withAttr (roleAttr $ leRole le)
          $ let ProjectName nm = lePName le
                mark f m = if f le then m else txt " "
                blk = mark leBlocking $ withAttr a'NoteWordBlocking $ txt "B"
                pnd = if lePending le > 0
                      then let decorate =
                                 case lePendDay le of
                                   Nothing -> withAttr a'NoteWordTODO
                                   Just d -> withDaysAttr (getToday gs) d
                           in decorate $ str (show $ lePending le)
                      else txt " "
                prg = mark leInProg $ txt "*"
                fut = mark leFuture $ withAttr a'NoteWordFuture $ txt "+"
            in txt nm <+> vLimit 1 (fill ' ') <+> blk <+> pnd <+> fut <+> prg
        lst = withVScrollBars OnRight $ renderList renderListEnt isFcsd (pL ps)
        srch = str "Search: " <+> renderEditor (txt . head) isFcsd (pS ps)
    in Just $ vBox [ lst, srch ]

  -- Pass the event to both the list and the search text box: since the search
  -- text box is only a single line, there is no conflict between the two
  -- widget's handling of events.
  handlePaneEvent _ ev ps = do
    -- Pass event to the list
    ps1 <- case ev of
             VtyEvent ev' ->
               ps & pList %%~ \w -> nestEventM' w (handleListEvent ev')
             _ -> return ps

    -- Pass event to the search list
    let origSearchText = head $ ps ^. pSrch . to getEditContents
    ps2 <- ps1 & pSrch %%~ \w -> nestEventM' w (handleEditorEvent ev)
    let searchText = head $ ps2 ^. pSrch . to getEditContents

    let ps3 =
          case ev of
            VtyEvent (Vty.EvKey Vty.KEnter []) ->
              -- this does nothing ordinarily, but if there is text in the search
              -- box, this will "select" the current element, returning the list
              -- to the full original contents and clearing the search box.
              let nl = oC ps2
                  np = if null nl then Nothing else Just 0
                  curElem = maybe id (listMoveToElement . snd)
                            $ listSelectedElement $ pL ps2
              in ps2
                 & pList %~ (curElem . listReplace (V.fromList nl) np)
                 & pSrch . editContentsL %~ TZ.clearZipper
            _ -> ps2


    -- If the search box text was updated, by this event, update the list
    -- accordingly.
    if searchText == origSearchText
      then return ps3
      else let nmtxt ent = let ProjectName pnt = lePName ent in pnt
               nc = if T.null searchText
                    then oC ps3
                    else filter ((searchText `T.isInfixOf`) . nmtxt) (oC ps3)
               np = if null nc then Nothing else Just 0
           in return $ ps3 & pList %~ listReplace (V.fromList nc) np

  focusable _ _ = Seq.singleton WProjList

  updatePane newprjs ps =
    let oc = DL.sort (mkListEnt <$> projects newprjs)
        curElem = maybe id listMoveTo $ listSelected $ pL ps
    in ps
       & pList %~ (curElem . listReplace (V.fromList oc) (Just 0))
       & pSrch . editContentsL %~ TZ.clearZipper
       & pOrig .~ oc


pList :: Lens' (PaneState Projects MyWorkEvent) (List WName ListEnt)
pList f ps = (\n -> ps { pL = n }) <$> f (pL ps)

pSrch :: Lens' (PaneState Projects MyWorkEvent) (Editor Text WName)
pSrch f ps = (\n -> ps { pS = n }) <$> f (pS ps)

pOrig :: Lens' (PaneState Projects MyWorkEvent) [ListEnt]
pOrig f ps = (\n -> ps { oC = n }) <$> f (oC ps)

mkListEnt :: Project -> ListEnt
mkListEnt pr =
  let keywords = catMaybes
                 (fst . noteKeyword <$> (concatMap notes $ locations pr))
      pendKW = \case
        TODO -> True
        -- FUTURE -> True -- handled by leFuture instead
        TODO_ _ -> True
        FUTURE_ _ -> True
        _ -> False
      pendDay = \case
        TODO_ d -> Just d
        FUTURE_ d -> Just d
        _ -> Nothing
  in LE { leRole = pr ^. roleL
        , lePName = pr ^. projNameL
        , leInProg = not $ null $ filter (== INPROG) keywords
        , lePending = toEnum $ length $ filter pendKW keywords
        , lePendDay = listToMaybe $ catMaybes $ DL.sort (pendDay <$> keywords)
        , leBlocking = not $ null $ filter (== BLOCKING) keywords
        , leFuture = not $ null $ filter (== FUTURE) keywords
        }


instance HasSelection (PaneState Projects MyWorkEvent) where
  selectedProject = fmap (lePName . snd) . listSelectedElement . pL
