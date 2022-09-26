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
       , newProjects :: Bool
         -- ^ True when myProjects has been updated; clear this via updatePane
       , errMsg :: String
       }
  type (DrawConstraints FileMgrPane s WName) = ( HasFocus s WName )
  type (EventConstraints FileMgrPane e) = ( HasFocus e WName )
  initPaneState _ = FB Nothing (Projects mempty) False ""
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
    AckNewProjects -> \ps -> ps { newProjects = False }
    UpdProject mbOldNm prj ->
      (myProjectsL %~ updateProject mbOldNm prj) . (newProjectsL .~ True)
    DelProject pname ->
      myProjectsL %~ Projects . filter ((/= pname) . name) . projects
    DelLocation pname locn ->
      let rmvLoc p = if name p == pname
                     then p { locations = filter (not . thisLoc) $ locations p }
                     else p
          thisLoc = ((== locn) . location)
      in myProjectsL %~ Projects . fmap rmvLoc . projects
    DelNote pname locn nt ->
      let rmvNote p = if name p == pname
                      then p { locations = rmvNote' <$> locations p }
                      else p
          rmvNote' l = if location l == locn
                       then l { notes = filter (not . thisNote) $ notes l }
                       else l
          thisNote = ((== nt) . noteTitle)
      in myProjectsL %~ Projects . fmap rmvNote . projects


fBrowser :: Lens' (PaneState FileMgrPane MyWorkEvent) (Maybe (FileBrowser WName))
fBrowser f ps = (\n -> ps { fB = n }) <$> f (fB ps)

myProjectsL :: Lens' (PaneState FileMgrPane MyWorkEvent) Projects
myProjectsL f wc = (\n -> wc { myProjects = n }) <$> f (myProjects wc)

newProjectsL :: Lens' (PaneState FileMgrPane MyWorkEvent) Bool
newProjectsL f wc = (\n -> wc { newProjects = n }) <$> f (newProjects wc)

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
                  else readProjectsFile fp >>= \case
                    Left er -> return $ ts & errMsgL .~ er
                    Right prjs -> return $ (ts { newProjects = True })
                                  & fBrowser .~ Nothing
                                  & myProjectsL .~ prjs
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
                    return $ ts & fBrowser .~ Nothing


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
  readProjectsFile f >>= \case
    Right prjs -> return $ (ps { newProjects = True }) & myProjectsL .~ prjs
    Left e -> error e


readProjectsFile :: MonadIO m => FilePath -> m (Either String Projects)
readProjectsFile fp = do
  newprjs <- eitherDecode <$> liftIO (BS.readFile fp)
  case newprjs of
    Right prjs -> Right . Projects <$> mapM syncProject (projects prjs)
    e@(Left _) -> return e


-- | Called to display the FileMgr modal pane to allow the user to Load or Save.
showFileMgr :: PaneState FileMgrPane MyWorkEvent
            -> IO (PaneState FileMgrPane MyWorkEvent)
showFileMgr prev = do
  dataDir <- takeDirectory <$> ensureDefaultProjectFile
  fb <- newFileBrowser selectNonDirectories (WName "FMgr:Browser") (Just dataDir)
  return $ prev & fBrowser .~ Just fb
