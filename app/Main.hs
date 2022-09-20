{-# LANGUAGE DataKinds #-}
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
import           Panes.Location ()
import           Panes.Notes
import           Panes.Operations
import           Panes.ProjInfo
import           Panes.Projects ()
import           Panes.Summary
import           Paths_mywork ( version )


type MyWorkState = Panel WName MyWorkEvent MyWorkCore
                   '[ SummaryPane
                    , AddProjPane
                    , OperationsPane
                    , ProjInfoPane
                    , NotesPane
                    , Location
                    , Projects
                    , FileMgrPane
                    , ConfirmationPane
                    ]

initialState :: MyWorkState
initialState = focusRingUpdate myWorkFocusL
               $ addToPanel Never
               $ addToPanel (WhenFocusedModalHandlingAllEvents Nothing)
               $ addToPanel Never
               $ addToPanel Never
               $ addToPanel WhenFocused
               $ addToPanel WhenFocused
               $ addToPanel WhenFocused
               $ addToPanel (WhenFocusedModal Nothing)
               $ addToPanel (WhenFocusedModalHandlingAllEvents Nothing)
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

          , (buttonAttr, defAttr `withStyle` dim)
          , (buttonSelectedAttr, black `on` green `withStyle` bold)

          , (a'RoleAuthor, fg $ rgbColor 0 255 (0::Int))
          , (a'RoleMaintainer, fg $ rgbColor 0 200 (30::Int))
          , (a'RoleContributor, fg $ rgbColor 0 145 (80::Int))
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
                 , pinfo >> panelDraw @NotesPane mws
                 ]
            ]
          , Just hBorder
          , panelDraw @OperationsPane mws
          ]
        ]
      allPanes = catMaybes [ panelDraw @FileMgrPane mws
                           , panelDraw @AddProjPane mws
                           , panelDraw @ConfirmationPane mws
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
    VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
    VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]) ->
      liftIO . Vty.refresh =<< getVtyHandle
    -- Other application global events (see Pane.Operations)
    VtyEvent (Vty.EvKey (Vty.KFun 1) []) -> do
      s <- get
      if s ^. onPane @FileMgrPane . to isFileMgrActive
      then return ()
      else do
        s' <- s & onPane @FileMgrPane %%~ liftIO . showFileMgr
        put $ s' & focusRingUpdate myWorkFocusL
    VtyEvent (Vty.EvKey (Vty.KFun 2) []) -> do
      s <- get
      put $ s & onPane @AddProjPane %~ initAddProj (snd $ getProjects s) Nothing
              & focusRingUpdate myWorkFocusL
    VtyEvent (Vty.EvKey (Vty.KChar 'e') [Vty.MCtrl]) -> do
      s <- get
      let fcsd = s ^. getFocus
      case (getCurrentLocation s, fcsd) of
        (Just (p, _), Focused (Just WProjList)) ->
          put $ s
          & onPane @AddProjPane %~ initAddProj (snd $ getProjects s) (Just p)
          & focusRingUpdate myWorkFocusL
        _ -> return ()
      return ()
    VtyEvent (Vty.EvKey Vty.KDel []) -> do
      s <- get
      let fcsd = s ^. getFocus
      case (getCurrentLocation s, fcsd) of
        (Just (p, _), Focused (Just WProjList)) ->
           put $ s & onPane @ConfirmationPane %~
                         showConfirmation (ConfirmProjectDelete (name p))
           & focusRingUpdate myWorkFocusL
        _ -> return ()

    -- Otherwise, allow the Panes in the Panel to handle the event.  The wrappers
    -- handle updates for any inter-state transitions.
    ev -> do
      changes <- handleLocationChange
                 $ handleProjectChange
                 $ handleNewProjects
                 $ handleNewProject
                 $ handleConfirmation
                 $ do s <- get
                      s' <- handleFocusAndPanelEvents myWorkFocusL s ev
                      put s'
                      return False
      when changes $ modify $ focusRingUpdate myWorkFocusL


handleConfirmation :: EventM WName MyWorkState Bool
                   -> EventM WName MyWorkState Bool
handleConfirmation innerHandler = do
  let confirmOp :: (PaneState ConfirmationPane MyWorkEvent -> a)
                -> EventM WName MyWorkState a
      confirmOp o = gets (view $ onPane @ConfirmationPane . to o)
  wasActive <- confirmOp isConfirmationActive
  forceChange <- innerHandler
  nowActive <- confirmOp isConfirmationActive
  if (wasActive && not nowActive)
    then do (ps, confirmed) <- confirmOp getConfirmedAction
            case confirmed of
              Nothing -> do modify $ onPane @ConfirmationPane .~ ps
                            return forceChange
              Just (ConfirmProjectDelete pname) -> do
                modify (   (onPane @FileMgrPane %~ updatePane (DelProject pname))
                         . (onPane @ConfirmationPane .~ ps)
                       )
                return True
    else return forceChange

handleNewProjects :: EventM WName MyWorkState Bool
                  -> EventM WName MyWorkState (Bool, Projects)
handleNewProjects innerHandler = do
  forceChange <- innerHandler
  (new,prjs) <- gets getProjects
  let mustUpdate = forceChange || new
  when mustUpdate $
    modify (   (onPane @Projects %~ updatePane prjs)
             . (onPane @FileMgrPane %~ updatePane AckNewProjects)
           )
  return (mustUpdate, prjs)

handleNewProject :: EventM WName MyWorkState Bool
                 -> EventM WName MyWorkState Bool
handleNewProject innerHandler = do
  wasActive <- gets (view $ onPane @AddProjPane . to isAddProjActive)
  forceChange <- innerHandler
  nowActive <- gets (view $ onPane @AddProjPane . to isAddProjActive)
  let changed = wasActive && not nowActive
  when changed $ modify $ \s ->
    let mbNewProj = s ^. onPane @AddProjPane . newProject
    in case mbNewProj of
         Just newProj ->
           s & onPane @FileMgrPane %~ updatePane (UpdProject newProj)
         Nothing -> s
  return (forceChange || changed)

handleProjectChange :: EventM WName MyWorkState (Bool, Projects)
                    -> EventM WName MyWorkState (Bool, Maybe Project)
handleProjectChange innerHandler = do
  pnm0 <- gets selectedProject
  (forceChange, prjs) <- innerHandler
  pnm <- gets selectedProject
  let mustUpdate = forceChange || pnm /= pnm0
  if mustUpdate
    then let p = DL.find ((== pnm) . Just . name) (projects prjs)
         in do modify $ onPane @Location %~ updatePane p
               return (mustUpdate, p)
    else return (mustUpdate, Nothing)

handleLocationChange :: EventM WName MyWorkState (Bool, Maybe Project)
                     -> EventM WName MyWorkState Bool
handleLocationChange innerHandler = do
  loc0 <- gets selectedLocation
  (forceChange, mbPrj) <- innerHandler
  loc1 <- gets selectedLocation
  let mustUpdate = forceChange || loc1 /= loc0
  case mbPrj of
    Nothing -> return ()
    Just p ->
      when mustUpdate $
        case DL.find ((== loc1) . Just . location) (locations p) of
          Just l -> modify $ onPane @NotesPane %~ updatePane (Just l)
          Nothing -> modify $ onPane @NotesPane %~ updatePane Nothing
  return mustUpdate


myWorkFocusL :: Lens' MyWorkState (FocusRing WName)
myWorkFocusL = onBaseState . coreWorkFocusL
