{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.Location () where

import           Brick hiding ( Location, on )
import           Brick.Panes
import           Brick.Widgets.List
import           Control.Lens
import           Data.Function ( on )
import qualified Data.List as DL
import           Data.Maybe ( fromMaybe )
import           Data.Time.Calendar
import qualified Data.Vector as V

import           Defs


instance Pane WName MyWorkEvent Location (Maybe Project) where
  data (PaneState Location MyWorkEvent) =
    L { lL :: List WName (LocationSpec, Bool, Maybe Day)
      , lP :: Maybe Project
      }
  type (InitConstraints Location s) = ( HasSelection s, HasProjects s )
  type (DrawConstraints Location s WName) = ( HasFocus s WName, HasSelection s )
  initPaneState gs =
    let l = L (list (WName "Loc:LList") mempty 1) Nothing
        update x = do p <- selectedProject gs
                      prj <- DL.find ((== p) . name)
                                     (projects $ snd $ getProjects gs)
                      return $ updatePane (Just prj) x
    in fromMaybe l $ update l
  drawPane ps gs =
    let isFcsd = gs^.getFocus.to focused == Just WLocations
        rndr (l,v,d) =
          let lattr = if v then id else withAttr a'Error
          in (lattr (str $ show $ l)
              <+> vLimit 1 (fill ' ')
              <+> (if v
                    then str $ maybe "*" show d
                    else withAttr a'Error $ str "INVALID"
                  )
             )
                     -- <=> str " "
    in Just
       $ withVScrollBars OnRight
       $ renderList (const rndr) isFcsd (lL ps)
  focusable _ ps = focus1If WLocations
                   $ not $ null $ listElements $ lL ps
  handlePaneEvent _ ev = lList %%~ \w -> nestEventM' w (handleListEvent ev)
  updatePane = \case
    Nothing -> (lList %~ listReplace mempty Nothing) . (lProj .~ Nothing)
    Just prj -> \ps ->
      let ents = [ (location l, locValid l, locatedOn l)
                 | l <- locations prj ]
          np = if null ents then Nothing else Just 0
          curElem = maybe id listMoveTo $ listSelected $ lL ps
          locOrd (l,v,d) = (d,l,v)
          sortLocs = DL.reverse . DL.sortBy (compare `on` locOrd)
      in ps
         & lList %~ (curElem . listReplace (V.fromList $ sortLocs ents) np)
         & lProj .~ Just prj


lList :: Lens' (PaneState Location MyWorkEvent)
         (List WName (LocationSpec, Bool, Maybe Day))
lList f ps = (\n -> ps { lL = n }) <$> f (lL ps)

lProj :: Lens' (PaneState Location MyWorkEvent) (Maybe Project)
lProj f ps = (\n -> ps { lP = n }) <$> f (lP ps)


instance HasLocation (PaneState Location MyWorkEvent) where
  selectedLocation ps = do
    prj <- lP ps
    curr <- listSelectedElement $ lL ps
    return ( name prj, (\(l,_,_) -> l) $ snd curr )
