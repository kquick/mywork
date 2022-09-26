{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Brick hiding ( Location )
import           Brick.Focus
import           Brick.Forms
import           Brick.Panes
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Brick.Widgets.Dialog
import           Control.Lens
import           Control.Monad ( when )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.List as DL
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Data.Version ( showVersion )
import           Graphics.Vty ( defAttr, withStyle, defaultStyleMask
                              , bold, reverseVideo, dim, underline
                              , black, white, yellow, red, green, rgbColor )
import qualified Graphics.Vty as Vty
import           System.Directory ( setCurrentDirectory )

import           Defs
import           Panes.AddProj
import           Panes.Confirmation
import           Panes.FileMgr
import           Panes.Help
import           Panes.Location ()
import           Panes.LocationInput
import           Panes.NoteInput
import           Panes.Notes ()
import           Panes.Operations
import           Panes.ProjInfo
import           Panes.Projects ()
import           Panes.Summary
import           Paths_mywork ( version )
import           Sync


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
                    ]

initialState :: MyWorkState
initialState = focusRingUpdate myWorkFocusL
               $ addToPanel Never
               $ addToPanel WhenFocusedModalHandlingAllEvents
               $ addToPanel WhenFocusedModalHandlingAllEvents
               $ addToPanel WhenFocusedModalHandlingAllEvents
               $ addToPanel Never
               $ addToPanel Never
               $ addToPanel WhenFocused
               $ addToPanel WhenFocused
               $ addToPanel WhenFocused
               $ addToPanel WhenFocusedModal
               $ addToPanel WhenFocusedModalHandlingAllEvents
               $ addToPanel WhenFocusedModal
               $ basePanel initMyWorkCore

main :: IO ()
main = do i <- initialState & onPane @FileMgrPane %%~ initFileMgr
          s <- defaultMain myworkApp i
          case getCurrentLocation s of
            Just (p,mbl) ->
              do putStrLn $ T.unpack $ name p <> ": " <> description p
                 case mbl of
                   Nothing -> return ()
                   Just l -> do
                     putStrLn $ T.unpack $ location l
                     setCurrentDirectory $ T.unpack $ location l
            Nothing -> return ()


myworkApp :: App MyWorkState MyWorkEvent WName
myworkApp = App { appDraw = drawMyWork
                , appChooseCursor = showFirstCursor
                , appHandleEvent = handleMyWorkEvent
                , appStartEvent =
                    -- Send move-up to Projects list pane.  The cursor should
                    -- already be at the top, but this invokes the various
                    -- wrappers that will update all the panes based on the
                    -- Projects loaded by initFileMgr
                    handleMyWorkEvent (VtyEvent (Vty.EvKey Vty.KUp []))
                , appAttrMap = const myattrs
                }

myattrs :: AttrMap
myattrs = attrMap defAttr
          [
            (editAttr, white `on` black)
          , (editFocusedAttr, yellow `on` black)

          , (listAttr, defAttr `withStyle` defaultStyleMask)
          , (listSelectedAttr, defAttr `withStyle` bold)
          , (listSelectedFocusedAttr, defAttr `withStyle` reverseVideo)

          , (invalidFormInputAttr, fg red `withStyle` bold)
          , (focusedFormInputAttr, defAttr `withStyle` reverseVideo)

          , (buttonAttr, black `on` (rgbColor 100 100 (100::Int)))
          , (buttonSelectedAttr, black `on` green)

          , (a'Operation, white `on` (rgbColor 0 1 (0::Int)))

          , (a'RoleAuthor, fg $ rgbColor 0 255 (0::Int))
          , (a'RoleMaintainer, fg $ rgbColor 0 200 (130::Int))
          , (a'RoleContributor, fg $ rgbColor 0 145 (180::Int))
          , (a'RoleUser, defAttr)

          , (a'ProjName, defAttr `withStyle` bold `withStyle` underline)

          , (a'Disabled, defAttr `withStyle` dim)
          , (a'Selected, black `on` yellow)
          , (a'Error, fg red)
          ]

drawMyWork :: MyWorkState -> [Widget WName]
drawMyWork mws =
  let mainPanes =
        [
          borderWithLabel  (str $ " mywork " <> showVersion version <> " ")
          $ vBox $ catMaybes
          [
            panelDraw @SummaryPane mws
          , Just hBorder
          , Just $ hBox $ catMaybes
            [ hLimitPercent 25
              <$> panelDraw @Projects mws
            , Just vBorder
            , Just $ vBox $ catMaybes $
              let pinfo = panelDraw @ProjInfoPane mws
              in [ pinfo
                 , const (hBorderWithLabel (str "Locations")) <$> pinfo
                 , panelDraw @Location mws
                 , const (hBorderWithLabel (str "Notes")) <$> pinfo
                 , pinfo >> panelDraw @Note mws
                 ]
            ]
          , Just hBorder
          , panelDraw @OperationsPane mws
          ]
        ]
      allPanes = catMaybes [ panelDraw @FileMgrPane mws
                           , panelDraw @AddProjPane mws
                           , panelDraw @LocationInputPane mws
                           , panelDraw @NoteInputPane mws
                           , panelDraw @Confirm mws
                           , panelDraw @HelpPane mws
                           ]
                 <> mainPanes
      disableLower = \case
        (m:ls) -> m : (withDefAttr a'Disabled <$> ls)
        o -> o
  in joinBorders . withBorderStyle unicode <$> disableLower allPanes


handleMyWorkEvent :: BrickEvent WName MyWorkEvent -> EventM WName MyWorkState ()
handleMyWorkEvent = \case
  AppEvent _ -> return () -- this app does not use these
    -- Application global actions
    --   * CTRL-q quits
    --   * CTRL-l refreshes vty
    --   * ESC dismisses any modal window
  VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> do
    s <- get
    if s ^. onPane @FileMgrPane . to unsavedChanges
      then modify ( (focusRingUpdate myWorkFocusL)
                    . (onPane @Confirm %~ showConfirmation ConfirmQuit)
                  )
      else halt
  VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]) ->
    liftIO . Vty.refresh =<< getVtyHandle
  -- Other application global events (see Pane.Operations)
  VtyEvent (Vty.EvKey (Vty.KFun 9) []) -> do
    s <- get
    if s ^. onPane @FileMgrPane . to isFileMgrActive
      then return ()
      else do
        s' <- s & onPane @FileMgrPane %%~ liftIO . showFileMgr
        put $ s' & focusRingUpdate myWorkFocusL

  -- Show help on F1
  VtyEvent (Vty.EvKey (Vty.KFun 1) []) ->
    modify $ onPane @HelpPane .~ initHelp

  -- Add an entry to the currently selected pane
  VtyEvent (Vty.EvKey (Vty.KFun 2) []) -> do
    s <- get
    case fst $ opOnSelection s of
      ProjectOp ->
        put $ s
        & onPane @AddProjPane %~ initAddProj (snd $ getProjects s) Nothing
        & focusRingUpdate myWorkFocusL
      LocationOp -> addLocation s
      NoteOp -> addNote s

  -- Add a sub-entry to the currently selected pane entry
  VtyEvent (Vty.EvKey (Vty.KFun 3) []) -> do
    s <- get
    case fst $ opOnSelection s of
      ProjectOp -> addLocation s
      LocationOp -> addNote s
      NoteOp -> addNote s  -- Note: F3 is not displayed, but no lower entry

  -- Edit the current selected entry in whichever pane is active
  VtyEvent (Vty.EvKey (Vty.KChar 'e') [Vty.MCtrl]) -> do
    s <- get
    case fst $ opOnSelection s of
      ProjectOp ->
        case getCurrentLocation s of
          Just (p, _) ->
            put $ s
            & onPane @AddProjPane %~ initAddProj (snd $ getProjects s) (Just p)
            & focusRingUpdate myWorkFocusL
          _ -> return ()
      LocationOp ->
        case getCurrentLocation s of
          Just (p, Just l) ->
            let n = name p
                ls = locations p
            in put $ s
               & onPane @LocationInputPane %~ initLocInput n ls (Just l)
               & focusRingUpdate myWorkFocusL
          _ -> return ()
      NoteOp ->
        case getCurrentLocation s of
          Just (_, Just l) ->
            let nt = getCurrentNote s l
            in do s' <- s & onPane @NoteInputPane %%~ initNoteInput (notes l) nt
                  put $ s' & focusRingUpdate myWorkFocusL
          _ -> return ()

  -- Delete the current selected entry in whichever pane is active
  VtyEvent (Vty.EvKey Vty.KDel []) -> do
    s <- get
    case fst $ opOnSelection s of
      ProjectOp ->
        case getCurrentLocation s of
          Just (p, _) ->
            put $ s
            & onPane @Confirm %~
                       showConfirmation (ConfirmProjectDelete (name p))
            & focusRingUpdate myWorkFocusL
          _ -> return ()
      LocationOp ->
        case getCurrentLocation s of
          Just (p, Just l) ->
            put $ s
            & onPane @Confirm %~
                 showConfirmation (ConfirmLocationDelete (name p) (location l))
            & focusRingUpdate myWorkFocusL
          _ -> return ()
      NoteOp ->
        case getCurrentLocation s of
          Just (p, Just l) ->
            case getCurrentNote s l of
              Just nt ->
                let msg = ConfirmNoteDelete (name p) (location l) (noteTitle nt)
                in put $ s & onPane @Confirm %~ showConfirmation msg
                           & focusRingUpdate myWorkFocusL
              _ -> return ()
          _ -> return ()

  -- Otherwise, allow the Panes in the Panel to handle the event.  The wrappers
  -- handle updates for any inter-state transitions.
  ev -> handleNoteInput
        $ handleLocationChange
        $ handleLocationInput
        $ handleProjectChange
        $ handleNewProjects
        $ handleNewProject
        $ handleConfirmation
        $ do s <- get
             (t,s') <- handleFocusAndPanelEvents myWorkFocusL s ev
             put s'
             return t


addLocation :: MyWorkState -> EventM WName MyWorkState ()
addLocation s =
  case getCurrentLocation s of
    Just (p,_) ->
      let n = name p
          ls = locations p
      in put $ s
         & onPane @LocationInputPane %~ initLocInput n ls Nothing
         & focusRingUpdate myWorkFocusL
    _ -> return ()


addNote :: MyWorkState -> EventM WName MyWorkState ()
addNote s =
  case getCurrentLocation s of
    Just (_, Just l) -> do
      s' <- s & onPane @NoteInputPane %%~ initNoteInput (notes l) Nothing
      put $ s' & focusRingUpdate myWorkFocusL
    _ -> return ()


handleConfirmation :: EventM WName MyWorkState PanelTransition
                   -> EventM WName MyWorkState PanelTransition
handleConfirmation innerHandler = do
  let confirmOp :: (PaneState Confirm MyWorkEvent -> a)
                -> EventM WName MyWorkState a
      confirmOp o = gets (view $ onPane @Confirm . to o)
  transition <- innerHandler
  deactivatedConfirmation <- gets (exitedModal @Confirm transition)
  when deactivatedConfirmation $
    do (ps, confirmed) <- confirmOp getConfirmedAction
       modify $ onPane @Confirm .~ ps
       let toFM msg = modify (onPane @FileMgrPane %~ updatePane msg)
       case confirmed of
         Nothing -> return ()
         Just (ConfirmProjectDelete pname) -> toFM $ DelProject pname
         Just (ConfirmLocationDelete pname l) -> toFM $ DelLocation pname l
         Just (ConfirmNoteDelete pname locn nt) -> toFM $ DelNote pname locn nt
         Just (ConfirmLoad fp) -> do
           s <- get
           s' <- s & onPane @FileMgrPane %%~ fileMgrReadProjectsFile fp
           put s'
         Just ConfirmQuit -> halt
  return transition

handleNewProject :: EventM WName MyWorkState PanelTransition
                 -> EventM WName MyWorkState PanelTransition
handleNewProject innerHandler = do
  let inpOp :: (PaneState AddProjPane MyWorkEvent -> a)
            -> EventM WName MyWorkState a
      inpOp o = gets (view $ onPane @AddProjPane . to o)
  transition <- innerHandler
  changed <- gets (exitedModal @AddProjPane transition)
  when changed $ do
    (mbOld, mbNewProj) <- inpOp projectInputResults
    case mbNewProj of
         Just newProj ->
           modify $ onPane @FileMgrPane %~ updatePane (UpdProject mbOld newProj)
         Nothing -> return ()
  return transition

handleNewProjects :: EventM WName MyWorkState PanelTransition
                  -> EventM WName MyWorkState (PanelTransition, Projects)
handleNewProjects innerHandler = do
  transition <- innerHandler
  (new,prjs) <- gets getProjects
  case new of
    Left cnfrm -> do modify $ \s ->
                       if not $ s ^. onPane @Confirm . to isConfirmationActive
                       then s
                            & onPane @Confirm %~ showConfirmation cnfrm
                            & onPane @FileMgrPane %~ updatePane AckNewProjects
                            & focusRingUpdate myWorkFocusL
                       else s
    Right True ->
      modify (   (onPane @Projects %~ updatePane prjs)
               . (onPane @FileMgrPane %~ updatePane AckNewProjects)
             )
    Right False -> return ()
  return (transition, prjs)  -- KWQ: viability of prjs if pending confirmation?

handleProjectChange :: EventM WName MyWorkState (PanelTransition, Projects)
                    -> EventM WName MyWorkState (PanelTransition, Maybe Project)
handleProjectChange innerHandler = do
  pnm0 <- gets selectedProject
  (transition, prjs) <- innerHandler
  pnm <- gets selectedProject
  let mustUpdate = pnm /= pnm0
  let p = DL.find ((== pnm) . Just . name) (projects prjs)
  when mustUpdate $ modify $ onPane @Location %~ updatePane p
  return (transition, p)


handleLocationInput :: EventM WName MyWorkState (PanelTransition, Maybe Project)
                    -> EventM WName MyWorkState (PanelTransition, Maybe Project)
handleLocationInput innerHandler = do
  let inpOp :: (PaneState LocationInputPane MyWorkEvent -> a)
            -> EventM WName MyWorkState a
      inpOp o = gets (view $ onPane @LocationInputPane . to o)
  (transition, mbPrj) <- innerHandler
  changed <- gets (exitedModal @LocationInputPane transition)
  if changed
    then do (mbOldL, mbNewLoc) <- inpOp locationInputResults
            case (mbNewLoc, mbPrj) of
              (Just newLoc, Just p) -> do
                locsts <- syncLocation newLoc
                let p' = updateLocation mbOldL (applyLocSync locsts newLoc) p
                    u = UpdProject Nothing p'
                modify (   (onPane @FileMgrPane %~ updatePane u)
                         . (onPane @Location %~ updatePane (Just p'))
                       )
                return (transition, Just p')
              _ -> return (transition, mbPrj)
    else return (transition, mbPrj)


handleLocationChange :: EventM WName MyWorkState (PanelTransition, Maybe Project)
                     -> EventM WName MyWorkState
                        (PanelTransition, Maybe Project, Maybe Location)
handleLocationChange innerHandler = do
  loc0 <- gets selectedLocation
  (transition, mbPrj) <- innerHandler
  loc1 <- gets selectedLocation
  let mustUpdate = loc1 /= loc0
  case mbPrj of
    Nothing -> return (transition, Nothing, Nothing)
    Just p -> do
      let mbl = DL.find ((== loc1) . Just . location) (locations p)
      when mustUpdate $ do
        case mbl of
          Just l -> modify $ onPane @Note %~ updatePane (Just l)
          Nothing -> modify $ onPane @Note %~ updatePane Nothing
      return (transition, mbPrj, mbl)

handleNoteInput :: EventM WName MyWorkState
                   (PanelTransition, Maybe Project, Maybe Location)
                -> EventM WName MyWorkState ()
handleNoteInput innerHandler = do
  let inpOp :: (PaneState NoteInputPane MyWorkEvent -> a)
            -> EventM WName MyWorkState a
      inpOp o = gets (view $ onPane @NoteInputPane . to o)
  (transition, mbPrj, mbLoc) <- innerHandler
  changed <- gets (exitedModal @NoteInputPane transition)
  when changed $
    do (mbOldN, mbNewNote) <- inpOp noteInputResults
       case (mbNewNote, mbPrj, mbLoc) of
         (Just newNote, Just p, Just l) ->
           let (p',l') = updateNote mbOldN newNote l p
               u = UpdProject Nothing p'
           in do modify ( (onPane @FileMgrPane %~ updatePane u)
                          . (onPane @Note %~ updatePane (Just l'))
                        )
         _ -> do return ()


myWorkFocusL :: Lens' MyWorkState (FocusRing WName)
myWorkFocusL = onBaseState . coreWorkFocusL
