{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.FileMgr
  (
    FileMgrPane
  , FileMgrOps(..)
  , initFileMgr
  , showFileMgr
  , isFileMgrActive
  , fileMgrReadProjectsFile
  , unsavedChanges
  )
where

import           Brick hiding ( Location )
import           Brick.Panes
import           Brick.Widgets.Center
import qualified Brick.Widgets.Core as BC
import           Brick.Widgets.FileBrowser
import qualified Control.Exception as X
import           Control.Lens
import           Control.Monad ( unless )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Aeson ( ToJSON, FromJSON, eitherDecode, encode
                            , parseJSON, withObject, (.:), (.:?), (.!=) )
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe ( isJust )
import qualified Data.Sequence as Seq
import           Data.Text ( Text )
import qualified Graphics.Vty as Vty
import qualified System.Directory as D
import           System.FilePath ( (</>), takeDirectory )

import           Defs
import           Panes.Common.Attrs
import           Sync


data FileMgrPane

data FileMgrOps = AckNewProjects
                | UpdProject (Maybe Text) Project -- add or replace project
                | DelProject Text
                | DelLocation Text Text
                | DelNote Text Text Text

instance Pane WName MyWorkEvent FileMgrPane FileMgrOps where
  data (PaneState FileMgrPane MyWorkEvent) =
    FB { fB :: Maybe (FileBrowser WName)
         -- ^ A Nothing value indicates the modal is not currently active
       , myProjects :: Projects
         -- ^ Current loaded set of projects
       , newProjects :: Either Confirm Bool
         -- ^ True when myProjects has been updated; clear this via updatePane
       , unsavedChanges :: Bool
         -- ^ True when myProjects has been changed since the last load
       , errMsg :: String
         -- ^ Internal error message to display in the FileMgr modal
       }
  type (DrawConstraints FileMgrPane s WName) = ( HasFocus s WName )
  type (EventConstraints FileMgrPane e) = ( HasFocus e WName )
  initPaneState _ = FB Nothing (Projects mempty) (Right False) False ""
  drawPane ps gs = drawFB ps gs <$> fB ps
  focusable _ ps = case fB ps of
                     Nothing -> mempty
                     Just _ -> Seq.fromList [ WName "FMgr:Browser"
                                            , WName "FMgr:SaveBtn"
                                            ]
  handlePaneEvent bs ev ts =
    let isSearching = maybe False fileBrowserIsSearching (ts^.fBrowser)
    in case ev of
      Vty.EvKey Vty.KEsc [] | not isSearching -> return $ ts & fBrowser .~ Nothing
      _ -> case bs^.getFocus of
             Focused (Just (WName "FMgr:Browser")) -> handleFileLoadEvent ev ts
             Focused (Just (WName "FMgr:SaveBtn")) -> handleFileSaveEvent ev ts
             _ -> return ts
  updatePane = \case
    AckNewProjects -> \ps -> ps { newProjects = Right False }
    UpdProject mbOldNm prj ->
      (myProjectsL %~ updateProject mbOldNm prj)
      . (newProjectsL .~ Right True)
      . (unsavedChangesL .~ True)
    DelProject pname ->
      (myProjectsL %~ Projects . filter ((/= pname) . name) . projects)
      . (unsavedChangesL .~ True)
    DelLocation pname locn ->
      let rmvLoc p = if name p == pname
                     then p { locations = filter (not . thisLoc) $ locations p }
                     else p
          thisLoc = ((== locn) . location)
      in (myProjectsL %~ Projects . fmap rmvLoc . projects)
         . (unsavedChangesL .~ True)
    DelNote pname locn nt ->
      let rmvNote p = if name p == pname
                      then p { locations = rmvNote' <$> locations p }
                      else p
          rmvNote' l = if location l == locn
                       then l { notes = filter (not . thisNote) $ notes l }
                       else l
          thisNote = ((== nt) . noteTitle)
      in (myProjectsL %~ Projects . fmap rmvNote . projects)
         . (unsavedChangesL .~ True)


fBrowser :: Lens' (PaneState FileMgrPane MyWorkEvent) (Maybe (FileBrowser WName))
fBrowser f ps = (\n -> ps { fB = n }) <$> f (fB ps)

myProjectsL :: Lens' (PaneState FileMgrPane MyWorkEvent) Projects
myProjectsL f wc = (\n -> wc { myProjects = n }) <$> f (myProjects wc)

newProjectsL :: Lens' (PaneState FileMgrPane MyWorkEvent) (Either Confirm Bool)
newProjectsL f wc = (\n -> wc { newProjects = n }) <$> f (newProjects wc)

unsavedChangesL :: Lens' (PaneState FileMgrPane MyWorkEvent) Bool
unsavedChangesL = lens unsavedChanges (\wc n -> wc { unsavedChanges = n })

errMsgL :: Lens' (PaneState FileMgrPane MyWorkEvent) String
errMsgL f wc = (\n -> wc { errMsg = n }) <$> f (errMsg wc)

isFileMgrActive :: PaneState FileMgrPane MyWorkEvent -> Bool
isFileMgrActive = isJust . fB


instance ( PanelOps FileMgrPane WName MyWorkEvent panes MyWorkCore
         , HasProjects (PaneState FileMgrPane MyWorkEvent)
         )
  => HasProjects (Panel WName MyWorkEvent MyWorkCore panes) where
  getProjects = getProjects . view (onPane @FileMgrPane)

instance HasProjects (PaneState FileMgrPane MyWorkEvent) where
  getProjects ps = (newProjects ps, myProjects ps)


drawFB :: DrawConstraints FileMgrPane drawstate WName
       => PaneState FileMgrPane MyWorkEvent
       -> drawstate -> FileBrowser WName -> Widget WName
drawFB ps ds b =
  let width = 70
      fcsd = ds^.getFocus.to focused
      browserPane fb =
        let hasFocus = fcsd == Just (WName "FMgr:Browser")
        in vLimitPercent 55 $ hLimitPercent width
           $ titledB hasFocus "Choose a file"
           $ renderFileBrowser hasFocus fb
      helpPane =
        padTop (BC.Pad 1) $ hLimitPercent width $ vBox
        [ hCenter $ txt "Up/Down: select"
        , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
        , hCenter $ txt "Enter: change directory or select file"
        , hCenter $ txt "Space: change directory"
        , hCenter $ txt "TAB: select Save option"
        , hCenter $ txt "ESC: quit"
        ]
      errDisplay fb = case fileBrowserException fb of
                        Nothing -> if null $ errMsg ps
                                   then emptyWidget
                                   else hLimitPercent width
                                        $ withDefAttr a'Error
                                        $ strWrap $ errMsg ps
                        Just e -> hLimitPercent width
                                  $ withDefAttr a'Error
                                  $ strWrap
                                  $ X.displayException e
      savePane = (if fcsd == Just (WName "FMgr:SaveBtn")
                  then withAttr a'Selected
                  else id)
                 $ str "[SAVE]"
  in centerLayer (browserPane b <=> errDisplay b <=> savePane <=> helpPane)


handleFileLoadEvent :: Vty.Event
                    -> PaneState FileMgrPane MyWorkEvent
                    -> EventM WName es (PaneState FileMgrPane MyWorkEvent)
handleFileLoadEvent ev ts =
  case ts^.fBrowser of
    Just fb -> do
      b <- nestEventM' fb $ handleFileBrowserEvent ev
      let selectFile =
            case fileBrowserCursor b of
              Nothing -> return $ ts & fBrowser .~ Just b  -- navigation
              Just f ->
                let fp = fileInfoFilePath f
                in liftIO $ D.doesDirectoryExist fp >>= \e ->
                  if e
                  then return $ ts & fBrowser .~ Just b
                  else if unsavedChanges ts
                       then let msg = ConfirmLoad fp
                            in return $ ts
                               & newProjectsL .~ Left msg
                               & fBrowser .~ Nothing
                       else (fBrowser .~ Nothing)
                            <$> fileMgrReadProjectsFile fp ts
      case ev of
        Vty.EvKey Vty.KEnter [] -> selectFile
        -- EvKey (KChar ' ') [] -> selectFile -- override filebrowser's default multi-select ability
        _ -> return $ ts & fBrowser .~ Just b & errMsgL .~ ""
    Nothing -> return ts  -- shouldn't happen


handleFileSaveEvent :: Vty.Event
                    -> PaneState FileMgrPane MyWorkEvent
                    -> EventM WName es (PaneState FileMgrPane MyWorkEvent)
handleFileSaveEvent ev ts =
  case fileBrowserCursor =<< ts^.fBrowser of
    Nothing -> return ts
    Just f ->
      case ev of
        Vty.EvKey Vty.KEnter [] -> doSave f
        Vty.EvKey (Vty.KChar ' ') [] -> doSave f
        _ -> return ts
  where
    doSave f =
      let fp = fileInfoFilePath f
      in liftIO (D.doesDirectoryExist fp) >>= \case
        True ->
          return $ ts
          & errMsgL .~ "Cannot save to a directory: please select a file"
        False -> do liftIO $ BS.writeFile fp (encode $ myProjects ts)
                    return $ ts
                      & fBrowser .~ Nothing
                      & unsavedChangesL .~ False


instance ToJSON Projects
instance ToJSON Project
instance ToJSON Group
instance ToJSON Role
instance ToJSON Language
instance ToJSON Location
instance ToJSON Note

instance FromJSON Projects
instance FromJSON Project where
  parseJSON = withObject "Project" $ \v -> Project
    <$> v .: "name"
    <*> v .:? "group" .!= Personal
    <*> v .: "role"
    <*> v .: "description"
    <*> v .: "language"
    <*> v .: "locations"

instance FromJSON Group
instance FromJSON Role
instance FromJSON Language
instance FromJSON Location where
  parseJSON = withObject "Location" $ \v -> Location
    <$> v .: "location"
    <*> v .: "locatedOn"
    <*> v .:? "locValid" .!= True -- assumed -- Added in v0.1.1.0
    <*> v .: "notes"
instance FromJSON Note
-- deriving via Generically Note instance ToJSON Note

ensureDefaultProjectFile :: IO FilePath
ensureDefaultProjectFile = do
  dataDir <- D.getXdgDirectory D.XdgData "mywork"
  D.createDirectoryIfMissing True dataDir
  let pFile = dataDir </> "projects.json"
  e <- D.doesFileExist pFile
  unless e $ BS.writeFile pFile ""
  return pFile


-- | Called to initialize the FileMgr at startup.  Reads the default Projects
-- file.
initFileMgr :: PaneState FileMgrPane MyWorkEvent
            -> IO (PaneState FileMgrPane MyWorkEvent)
initFileMgr ps = do
  f <- ensureDefaultProjectFile
  ps' <- fileMgrReadProjectsFile f ps
  let e = ps' ^. errMsgL
  unless (null e) $ error e
  return ps'


readProjectsFile :: MonadIO m => FilePath -> m (Either String Projects)
readProjectsFile fp = do
  newprjs <- eitherDecode <$> liftIO (BS.readFile fp)
  case newprjs of
    Right prjs -> Right . Projects <$> mapM syncProject (projects prjs)
    e@(Left _) -> return e

fileMgrReadProjectsFile :: MonadIO m
                        => FilePath
                        -> PaneState FileMgrPane MyWorkEvent
                        -> m (PaneState FileMgrPane MyWorkEvent)
fileMgrReadProjectsFile fp ps = readProjectsFile fp >>= \case
  Left er -> return $ ps & errMsgL .~ er
  Right prjs -> return $ ps
                & myProjectsL .~ prjs
                & newProjectsL .~ Right True
                & unsavedChangesL .~ False


-- | Called to display the FileMgr modal pane to allow the user to Load or Save.
showFileMgr :: PaneState FileMgrPane MyWorkEvent
            -> IO (PaneState FileMgrPane MyWorkEvent)
showFileMgr prev =
  case prev ^. fBrowser of
    Just _ -> return prev
    Nothing -> do
      let n = WName "FMgr:Browser"
      dataDir <- takeDirectory <$> ensureDefaultProjectFile
      fb <- newFileBrowser selectNonDirectories n (Just dataDir)
      return $ prev & fBrowser .~ Just fb
