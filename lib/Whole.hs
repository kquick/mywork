{-# LANGUAGE DataKinds #-}

module Whole
  (
    MyWorkState
  , initialState
  , myWorkFocusL
  )
where

import Brick.Focus ( FocusRing )
import Brick.Panes
import Control.Lens ( Lens' )

import Defs
import Panes.AddProj
import Panes.Confirmation ()
import Panes.FileMgr
import Panes.Help
import Panes.Location ()
import Panes.LocationInput
import Panes.Messages
import Panes.NoteInput
import Panes.Notes ()
import Panes.Operations
import Panes.ProjInfo
import Panes.Projects ()
import Panes.Summary


type MyWorkState = Panel WName MyWorkEvent MyWorkCore
                   '[ SummaryPane
                    , NoteInputPane
                    , LocationInputPane
                    , AddProjPane
                    , OperationsPane
                    , ProjInfoPane
                    , Note
                    , Location
                    , Projects
                    , FileMgrPane
                    , Confirm
                    , HelpPane
                    , MessagesPane
                    ]


myWorkFocusL :: Lens' MyWorkState (FocusRing WName)
myWorkFocusL = onBaseState . coreWorkFocusL


initialState :: IO MyWorkState
initialState = focusRingUpdate myWorkFocusL
               . addToPanel Never
               . addToPanel WhenFocusedModalHandlingAllEvents
               . addToPanel WhenFocusedModalHandlingAllEvents
               . addToPanel WhenFocusedModalHandlingAllEvents
               . addToPanel Never
               . addToPanel Never
               . addToPanel WhenFocused
               . addToPanel WhenFocused
               . addToPanel WhenFocused
               . addToPanel WhenFocusedModal
               . addToPanel WhenFocusedModalHandlingAllEvents
               . addToPanel WhenFocusedModal
               . addToPanel Never
               . basePanel
               <$> initMyWorkCore
