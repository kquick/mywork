{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Panes.Operations
  (
    OperationsPane
  )
where

import Brick
import           Brick.Panes
import qualified Data.List as List

import           Defs


data OperationsPane

instance Pane WName MyWorkEvent OperationsPane () where
  data (PaneState OperationsPane MyWorkEvent) = Unused
  type (DrawConstraints OperationsPane s WName) = ( HasSelection s )
  initPaneState _ = Unused
  drawPane _ gs =
    let projInd = case selectedProject gs of
                    Nothing -> withAttr a'Disabled
                    Just _ -> id
        ops = List.intersperse (fill ' ')
              [ str "F1: Load/Save"
              , str "F2: Add Project"
              , projInd $ str "F3: Add Location"
              , projInd $ str "F4: Add Note"
              , projInd $ str "C-e: Edit"
              , projInd $ str "Del: Delete"
              ]
    in Just $ vLimit 1 $ str " " <+> hBox ops <+> str " "
