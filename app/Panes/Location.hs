{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.Location () where

import           Brick hiding ( Location )
import           Brick.Panes
import           Brick.Widgets.List
import           Control.Lens
import qualified Data.List as DL
import           Data.Maybe ( fromMaybe )
import           Data.Text ( Text )
import           Data.Time.Calendar
import qualified Data.Vector as V

import           Defs


instance Pane WName MyWorkEvent Location (Maybe Project) where
  data (PaneState Location MyWorkEvent) =
    L { lL :: List WName (Text, Bool, Maybe Day) }
  type (InitConstraints Location s) = ( HasSelection s, HasProjects s )
  type (DrawConstraints Location s WName) = ( HasFocus s WName, HasSelection s )
  initPaneState gs =
    let l = L (list (WName "Loc:LList") mempty 1)
        update x = do p <- selectedProject gs
                      prj <- DL.find ((== p) . name)
                                     (projects $ snd $ getProjects gs)
                      return $ updatePane (Just prj) x
    in fromMaybe l $ update l
  drawPane ps gs =
    let isFcsd = gs^.getFocus.to focused == Just WLocations
        rndr (l,v,d) =
          let lattr = if v then id else withAttr a'Error
          in (lattr (txt l)
              <+> vLimit 1 (fill ' ')
              <+> (if v
                    then str $ maybe "*" show d
                    else withAttr a'Error $ str "INVALID"
                  )
             )
                     -- <=> str " "
    in Just $ renderList (const rndr) isFcsd (lL ps)
  focusable _ ps = focus1If WLocations
                   $ not $ null $ listElements $ lL ps
  handlePaneEvent _ ev = lList %%~ \w -> nestEventM' w (handleListEvent ev)
  updatePane = \case
    Nothing -> lList %~ listReplace mempty Nothing
    Just prj -> let ents = [ (location l, locValid l, locatedOn l)
                           | l <- locations prj ]
                    np = if null ents then Nothing else Just 0
                in lList %~ listReplace (V.fromList ents) np


lList :: Lens' (PaneState Location MyWorkEvent) (List WName (Text, Bool, Maybe Day))
lList f ps = (\n -> ps { lL = n }) <$> f (lL ps)


instance HasLocation (PaneState Location MyWorkEvent) where
  selectedLocation = fmap (\(l,_,_) -> l) . fmap snd . listSelectedElement . lL
