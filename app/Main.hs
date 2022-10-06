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
import           Brick.Widgets.Dialog
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Control.Lens
import           Control.Monad ( unless, when )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Reader ( ReaderT, runReaderT, ask, lift )
import           Control.Monad.Writer ( WriterT, execWriterT, tell )
import qualified Data.List as DL
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Data.Version ( showVersion )
import           Graphics.Vty ( defAttr, withStyle, defaultStyleMask
                              , bold, reverseVideo, dim, underline
                              , black, white, yellow, red, green, cyan, rgbColor )
import qualified Graphics.Vty as Vty

import           Defs
import           Panes.AddProj
import           Panes.Confirmation
import           Panes.FileMgr
import           Panes.Help
import           Panes.Location ()
import           Panes.LocationInput
import           Panes.Messages
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
                    , MessagesPane
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
               $ addToPanel Never
               $ basePanel initMyWorkCore

main :: IO ()
main = do i <- initialState & onPane @FileMgrPane %%~ initFileMgr
          s <- defaultMain myworkApp i
          case getCurrentLocation s of
            Just (p,mbl) ->
              do let ProjectName pnm = name p
                 putStrLn $ T.unpack $ pnm <> ": " <> description p
                 case mbl of
                   Nothing -> return ()
                   Just l ->
                     case location l of
                       RemoteSpec r ->
                         putStrLn $ "Remote location: " <> T.unpack r
                       LocalSpec d -> do putStrLn "Local directory"
                                         putStrLn $ show d

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
          , (a'Notice, fg cyan)
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
            [ hLimit 25
              <$> panelDraw @Projects mws
            , Just vBorder
            , Just $ vBox $ catMaybes $
              let pinfo = panelDraw @ProjInfoPane mws
              in [ pinfo
                 , const (hBorderWithLabel (str "Locations")) <$> pinfo
                 , vLimitPercent 28 <$> panelDraw @Location mws
                 , const (hBorderWithLabel (str "Notes")) <$> pinfo
                 , pinfo >> panelDraw @Note mws
                 ]
            ]
          , Just hBorder
          , panelDraw @MessagesPane mws
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

  --------------------------------------------------------
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

  --------------------------------------------------------
  -- Other application global events (see Pane.Operations)

  -- Enter Load/Save modal dialog
  VtyEvent (Vty.EvKey (Vty.KFun 9) []) -> do
    resetMessages
    s <- get
    if s ^. onPane @FileMgrPane . to isFileMgrActive
      then return ()
      else do
        s' <- s & onPane @FileMgrPane %%~ liftIO . showFileMgr
        put $ s' & focusRingUpdate myWorkFocusL

  -- Quickly save to current file (if possible)
  ev@(VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl])) -> do
    s <- get
    when (not $ s ^. onPane @FileMgrPane . to isFileMgrActive) $ do
      s' <- s & onPane @FileMgrPane %%~ liftIO . showFileMgr
      put $ s' & focusRingUpdate myWorkFocusL
    eventToPanel ev
    return ()

  -- Show help on F1
  VtyEvent (Vty.EvKey (Vty.KFun 1) []) ->
    modify $ onPane @HelpPane .~ initHelp

  -- Add an entry to the currently selected pane
  VtyEvent (Vty.EvKey (Vty.KFun 2) []) -> do
    resetMessages
    s <- get
    case opOnSelection s of
      ProjectOp ->
        put $ s
        & onPane @AddProjPane %~ initAddProj (snd $ getProjects s) Nothing
        & focusRingUpdate myWorkFocusL
      LocationOp -> addLocation s
      NoteOp -> addNote s

  -- Add a sub-entry to the currently selected pane entry
  VtyEvent (Vty.EvKey (Vty.KFun 3) []) -> do
    resetMessages
    s <- get
    case opOnSelection s of
      ProjectOp -> addLocation s
      LocationOp -> addNote s
      NoteOp -> addNote s  -- Note: F3 is not displayed, but no lower entry

  -- Edit the current selected entry in whichever pane is active
  VtyEvent (Vty.EvKey (Vty.KChar 'e') [Vty.MCtrl]) -> do
    isModal <- gets (isPanelModal myWorkFocusL)
    unless isModal $ do
      resetMessages
      s <- get
      case opOnSelection s of
        ProjectOp ->
          case getCurrentLocation s of
            Just (p, _) ->
              put $ s
              & onPane @AddProjPane %~ initAddProj (snd $ getProjects s) (Just p)
              & focusRingUpdate myWorkFocusL
            _ -> return ()
        LocationOp ->
          case getCurrentLocation s of
            Just (p, Just l) ->  -- KWQ: same as addLocation but Just l ...
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
    isModal <- gets (isPanelModal myWorkFocusL)
    unless isModal $ do
      resetMessages
      s <- get
      let cnf =
            case opOnSelection s of
              ProjectOp ->
                case getCurrentLocation s of
                  Just (p, _) -> Just $ ConfirmProjectDelete (name p)
                  _ -> Nothing
              LocationOp ->
                case getCurrentLocation s of
                  Just (p, Just l) ->
                    Just $ ConfirmLocationDelete (name p) (location l)
                  _ -> Nothing
              NoteOp -> do (p, mbl) <- getCurrentLocation s
                           l <- mbl
                           nt <- noteTitle <$> getCurrentNote s l
                           pure $ ConfirmNoteDelete (name p) (location l) nt
      case cnf of
        Just cmsg -> put $ s & onPane @Confirm %~ showConfirmation cmsg
                             & focusRingUpdate myWorkFocusL
        Nothing -> return ()

  -- Otherwise, allow the Panes in the Panel to handle the event.  The wrappers
  -- handle updates for any inter-state transitions.
  ev -> eventToPanel ev

  where
    eventToPanel ev = do
      resetMessages
      s <- get
      (t,s') <- handleFocusAndPanelEvents myWorkFocusL s ev
      put s'
      when (exitedModal @FileMgrPane t s') $
             let fmn = Just $ (panelState @FileMgrPane s') ^. fileMgrNotices
             in modify
                (   (onPane @FileMgrPane . fileMgrNotices .~ mempty)
                  . (onPane @MessagesPane %~ updatePane fmn)
                )
      let postop = do handleConfirmation
                      handleNewProject
                      handleProjectChanges
                      mbprj <- handleProjectChange
                      mbprj' <- handleLocationInput mbprj
                      mbloc <- handleLocationChange mbprj'
                      handleNoteInput mbprj' mbloc
      refocus <- execWriterT $ runReaderT postop t
      when (or refocus) $ modify $ focusRingUpdate myWorkFocusL


addLocation :: MyWorkState -> EventM WName MyWorkState ()
addLocation s =
  case getCurrentProject s of
    Just p ->
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


resetMessages :: EventM WName MyWorkState ()
resetMessages = modify $ onPane @MessagesPane %~ updatePane Nothing


type PostOpM a = ReaderT PanelTransition (WriterT [Bool] (EventM WName MyWorkState)) a

handleConfirmation :: PostOpM ()
handleConfirmation = do
  let confirmOp :: (PaneState Confirm MyWorkEvent -> a) -> PostOpM a
      confirmOp o = gets (view $ onPane @Confirm . to o)
  transition <- ask
  deactivatedConfirmation <- gets (exitedModal @Confirm transition)
  when deactivatedConfirmation $
    do (ps, confirmed) <- confirmOp getConfirmedAction
       modify $ onPane @Confirm .~ ps
       let toFM msg = do modify (onPane @FileMgrPane %~ updatePane msg)
                         tell [True]
       case confirmed of
         Nothing -> return ()
         Just (ConfirmProjectDelete pname) -> toFM $ DelProject pname
         Just (ConfirmLocationDelete pname l) ->
           do toFM $ DelLocation pname l
              p <- gets getCurrentProject -- updated by DelLocation above
              modify (onPane @Location %~ updatePane p)
         Just (ConfirmNoteDelete pname locn nt) ->
           do toFM $ DelNote pname locn nt
              cl <- gets getCurrentLocation -- updated by DelNote
              let mbl = do (_,mbl') <- cl
                           mbl'
              modify (onPane @Note %~ updatePane mbl)
         Just (ConfirmLoad fp) -> do
           s <- get
           s' <- s & onPane @FileMgrPane %%~ fileMgrReadProjectsFile fp
           put s'
         Just ConfirmQuit -> lift $ lift halt

handleNewProject :: PostOpM ()
handleNewProject = do
  let inpOp :: (PaneState AddProjPane MyWorkEvent -> a) -> PostOpM a
      inpOp o = gets (view $ onPane @AddProjPane . to o)
  transition <- ask
  changed <- gets (exitedModal @AddProjPane transition)
  when changed $ do
    (mbOld, mbNewProj) <- inpOp projectInputResults
    case mbNewProj of
         Just newProj ->
           modify $ onPane @FileMgrPane %~ updatePane (UpdProject mbOld newProj)
         Nothing -> return ()

handleProjectChanges :: PostOpM ()
handleProjectChanges = lift $ do
  (changed,prjs) <- gets getProjects
  case changed of
    Left cnfrm -> do modify $ \s ->
                       if not $ s ^. onPane @Confirm . to isConfirmationActive
                       then s
                            & onPane @Confirm %~ showConfirmation cnfrm
                            & onPane @FileMgrPane %~ updatePane AckProjectChanges
                            & focusRingUpdate myWorkFocusL
                       else s
    Right True ->
      modify (   (onPane @Projects %~ updatePane prjs)
               . (onPane @FileMgrPane %~ updatePane AckProjectChanges)
             )
    Right False -> return ()


handleProjectChange :: PostOpM (Maybe Project)
handleProjectChange = do
  mbp <- gets getCurrentProject -- from ProjList pane
  pnm <- gets (fmap fst . selectedLocation) -- from Location pane
  let mustUpdate = pnm /= (name <$> mbp)
  when mustUpdate $ do modify $ onPane @Location %~ updatePane mbp
                       tell [True]
  return mbp

handleLocationInput :: Maybe Project -> PostOpM (Maybe Project)
handleLocationInput mbPrj = do
  let inpOp :: (PaneState LocationInputPane MyWorkEvent -> a) -> PostOpM a
      inpOp o = lift $ gets (view $ onPane @LocationInputPane . to o)
  transition <- ask
  changed <- gets (exitedModal @LocationInputPane transition)
  if changed
    then do (mbOldL, mbNewLoc) <- inpOp locationInputResults
            case (mbNewLoc, mbPrj) of
              (Just newLoc, Just p) -> do
                p' <- applyProjLocSync mbOldL p newLoc
                let u = UpdProject Nothing p'
                modify (   (onPane @FileMgrPane %~ updatePane u)
                         . (onPane @Location %~ updatePane (Just p'))
                       )
                return $ Just p'
              _ -> return mbPrj
    else return mbPrj


handleLocationChange :: Maybe Project -> PostOpM (Maybe Location)
handleLocationChange = \case
  Nothing -> return Nothing
  Just p -> do
    loc0 <- gets (fmap snd . selectedLocation) -- Location pane
    loc1 <- gets (fmap fst . selectedNote) -- Notes pane
    let mbl = DL.find ((== loc0) . Just . location) (locations p)
    unless (loc0 == loc1) $ do modify $ onPane @Note %~ updatePane mbl
                               tell [True]
    return mbl

handleNoteInput :: Maybe Project -> Maybe Location -> PostOpM ()
handleNoteInput mbPrj mbLoc = do
  let inpOp :: (PaneState NoteInputPane MyWorkEvent -> a) -> PostOpM a
      inpOp o = lift $ gets (view $ onPane @NoteInputPane . to o)
  transition <- ask
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
