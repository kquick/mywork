{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.Location
  (
    locationPane
  )
where

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


instance Pane WName MyWorkEvent Location Project where
  data (PaneState Location MyWorkEvent) = L { lL :: List WName (Text, Maybe Day) }
  type (InitConstraints Location s) = ( HasSelection s, HasProjects s )
  type (DrawConstraints Location s WName) = ( HasFocus s WName, HasSelection s )
  initPaneState gs =
    let l = L (list WLList mempty 2)
        update x = do p <- selectedProject gs
                      prj <- DL.find ((== p) . name)
                                     (projects $ snd $ getProjects gs)
                      return $ updatePane prj x
    in fromMaybe l $ update l
  drawPane ps gs =
    let isFcsd = gs^.getFocus.to focused == Just WLocation
        rndr (l,d) = (txt l
                      <+> vLimit 1 (fill ' ')
                      <+> (str $ maybe "*" show d)
                     )
                     <=> str " "
    in Just $ renderList (const rndr) isFcsd (lL ps)
  focusable _ ps = focus1If WLocation $ not $ null $ listElements $ lL ps
  handlePaneEvent _ ev = lList %%~ \w -> nestEventM' w (handleListEvent ev)
  updatePane prj ps =
    let ents = [ (location l, locatedOn l) | l <- locations prj ]
    in L $ listReplace (V.fromList ents) (Just 0) (lL ps)


lList :: Lens' (PaneState Location MyWorkEvent) (List WName (Text, Maybe Day))
lList f ps = (\n -> ps { lL = n }) <$> f (lL ps)


locationPane :: Location
locationPane = undefined
