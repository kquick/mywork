{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Panes.Operations
  (
    OperationsPane
  )
where

import           Brick
import           Brick.Panes
import qualified Data.List as DL
import           Data.Maybe ( catMaybes )

import           Defs


data OperationsPane

instance Pane WName MyWorkEvent OperationsPane () where
  data (PaneState OperationsPane MyWorkEvent) = Unused
  type (DrawConstraints OperationsPane s WName) = ( HasSelection s
                                                  , HasProjects s
                                                  , HasLocation s
                                                  , HasFocus s WName
                                                  )
  initPaneState _ = Unused
  drawPane _ gs =
    let (o,t) = opOnSelection gs
        projInd = case t of
                    Nothing -> withAttr a'Disabled
                    Just _ -> id
        addWhat x = case x of
                      ProjectOp -> "Project"
                      LocationOp -> "Location"
                      NoteOp -> "Note"
        ops = DL.intersperse (fill ' ')
              $ withAttr a'Operation
              <$> catMaybes
              [
                Just $ str $ "F1 Help"
              , Just $ str $ "F2 Add " <> addWhat o
              , if o == maxBound
                then Nothing
                else Just $ projInd $ str $ "F3 Add " <> addWhat (succ o)
              -- , projInd $ str "F4: Add Note"
              , Just $ projInd $ str "C-e Edit"
              , Just $ projInd $ str "Del Delete"
              , Just $ str "F9 Load/Save"
              , Just $ str $ "C-q Quit"
              ]
    in Just $ vLimit 1 $ str " " <+> hBox ops <+> str " "
