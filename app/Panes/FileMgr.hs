{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
  , fileMgrNotices
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
import           Data.Aeson ( ToJSON, FromJSON , eitherDecode, encode, toJSON
                            , genericParseJSON, defaultOptions, genericToJSON
                            , SumEncoding(UntaggedValue), sumEncoding, Options
                            , parseJSON, withObject, (.:), (.:?), (.!=) )
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe ( catMaybes )
import           Data.Maybe ( isJust )
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import           Path ( Path, Abs, File, (</>), parseAbsFile
                      , relfile, reldir, parent, toFilePath )
import           Path.IO ( createDirIfMissing, doesFileExist
                         , XdgDirectory(XdgData), getXdgDir )

import           Defs
import           Panes.Common.Attrs
import           Sync


data FileMgrPane

data FileMgrOps = AckProjectChanges
                | UpdProject (Maybe ProjectName) Project -- add/replace project
                | DelProject ProjectName
                | DelLocation ProjectName LocationSpec
                | DelNote ProjectName LocationSpec NoteTitle

instance Pane WName MyWorkEvent FileMgrPane FileMgrOps where
  data (PaneState FileMgrPane MyWorkEvent) =
    FB { fB :: Maybe (FileBrowser WName)
         -- ^ A Nothing value indicates the modal is not currently active
       , myProjects :: Projects
         -- ^ Current loaded set of projects
       , myProjFile :: Maybe (Path Abs File)
       , projsChanged :: Either Confirm Bool
         -- ^ True when myProjects has been updated; clear this via updatePane
       , unsavedChanges :: Bool
         -- ^ True when myProjects has been changed since the last load
       , errMsg :: String
         -- ^ Internal error message to display in the FileMgr modal
       , fmgrMsgs :: [ Widget WName ]
         -- ^ Messages to display when the FileMgr modal exits
       }
  type (DrawConstraints FileMgrPane s WName) = ( HasFocus s WName )
  type (EventConstraints FileMgrPane e) = ( HasFocus e WName )
  initPaneState _ = FB Nothing (Projects mempty) Nothing (Right False)
                    False "" mempty
  drawPane ps gs = drawFB ps gs <$> fB ps
  focusable _ ps = case fB ps of
                     Nothing -> mempty
                     Just _ -> Seq.fromList $ catMaybes
                               [
                                 Just $ WName "FMgr:Browser"
                               , const (WName "FMgr:SaveBtn") <$> myProjFile ps
                               , if isDirSelected ps
                                 then Nothing
                                 else Just $ WName "FMgr:SaveAsBtn"
                               ]
  handlePaneEvent bs ev ts =
    let isSearching = maybe False fileBrowserIsSearching (ts^.fBrowser)
    in case ev of
      Vty.EvKey Vty.KEsc [] | not isSearching -> return $ ts & fBrowser .~ Nothing
      Vty.EvKey (Vty.KChar 's') [Vty.MCtrl] -> do
        ts' <- case myProjFile ts of
          Just fp -> fileMgrSaveProjectsFile fp ts
          Nothing ->
            -- Ctrl-S dismisses the FileMgr window, even on failure
            return ts
        return $ ts'
          & exitMsgsL <>~ (if null (ts ^. errMsgL) then []
                           else [ withAttr a'Error $ str $ ts ^. errMsgL])
          & fBrowser .~ Nothing
      _ -> case bs^.getFocus of
             Focused (Just (WName "FMgr:Browser")) -> handleFileLoadEvent ev ts
             Focused (Just (WName "FMgr:SaveAsBtn")) -> handleFileSaveEvent ev ts
             Focused (Just (WName "FMgr:SaveBtn")) ->
               case myProjFile ts of
                 Just fp -> fileMgrSaveProjectsFile fp ts
                 Nothing -> return ts
             _ -> return ts
  updatePane = \case
    AckProjectChanges ->
      \ps -> ps { projsChanged = Right False, fmgrMsgs = mempty }
    UpdProject mbOldNm prj ->
      (myProjectsL %~ updateProject mbOldNm prj)
      . (projsChangedL .~ Right True)
      . (unsavedChangesL .~ True)
    DelProject pname ->
      (myProjectsL %~ Projects . filter ((/= pname) . name) . projects)
      . (projsChangedL .~ Right True)
      . (unsavedChangesL .~ True)
    DelLocation pname locn ->
      let rmvLoc p = if name p == pname
                     then p { locations = filter (not . thisLoc) $ locations p }
                     else p
          thisLoc = ((== locn) . location)
      in (myProjectsL %~ Projects . fmap rmvLoc . projects)
         . (projsChangedL .~ Right True)
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
         . (projsChangedL .~ Right True)
         . (unsavedChangesL .~ True)


fBrowser :: Lens' (PaneState FileMgrPane MyWorkEvent) (Maybe (FileBrowser WName))
fBrowser f ps = (\n -> ps { fB = n }) <$> f (fB ps)

myProjectsL :: Lens' (PaneState FileMgrPane MyWorkEvent) Projects
myProjectsL f wc = (\n -> wc { myProjects = n }) <$> f (myProjects wc)

myProjFileL :: Lens' (PaneState FileMgrPane MyWorkEvent) (Maybe (Path Abs File))
myProjFileL f wc = (\n -> wc { myProjFile = n }) <$> f (myProjFile wc)

projsChangedL :: Lens' (PaneState FileMgrPane MyWorkEvent) (Either Confirm Bool)
projsChangedL f wc = (\n -> wc { projsChanged = n }) <$> f (projsChanged wc)

unsavedChangesL :: Lens' (PaneState FileMgrPane MyWorkEvent) Bool
unsavedChangesL = lens unsavedChanges (\wc n -> wc { unsavedChanges = n })

errMsgL :: Lens' (PaneState FileMgrPane MyWorkEvent) String
errMsgL f wc = (\n -> wc { errMsg = n }) <$> f (errMsg wc)

exitMsgsL :: Lens' (PaneState FileMgrPane MyWorkEvent) [ Widget WName ]
exitMsgsL f wc = (\n -> wc { fmgrMsgs = n }) <$> f (fmgrMsgs wc)

isFileMgrActive :: PaneState FileMgrPane MyWorkEvent -> Bool
isFileMgrActive = isJust . fB

fileMgrNotices :: Lens' (PaneState FileMgrPane MyWorkEvent) [ Widget WName ]
fileMgrNotices = exitMsgsL

isDirSelected :: PaneState FileMgrPane MyWorkEvent -> Bool
isDirSelected ps =
  case fileBrowserCursor =<< fB ps of
    Just fi ->
      case fileStatusFileType <$> fileInfoFileStatus fi of
        Right (Just Directory) -> True
        Right (Just SymbolicLink) -> case fileInfoLinkTargetType fi of
                                       Just Directory -> True
                                       _ -> False
        _ -> False
    Nothing -> False


instance ( PanelOps FileMgrPane WName MyWorkEvent panes MyWorkCore
         , HasProjects (PaneState FileMgrPane MyWorkEvent)
         )
  => HasProjects (Panel WName MyWorkEvent MyWorkCore panes) where
  getProjects = getProjects . view (onPane @FileMgrPane)

instance HasProjects (PaneState FileMgrPane MyWorkEvent) where
  getProjects ps = (projsChanged ps, myProjects ps)


drawFB :: DrawConstraints FileMgrPane drawstate WName
       => PaneState FileMgrPane MyWorkEvent
       -> drawstate -> FileBrowser WName -> Widget WName
drawFB ps ds b =
  let width = 70
      fcsd = ds^.getFocus.to focused
      browserFocused = fcsd == Just (WName "FMgr:Browser")
      isDir = isDirSelected ps
      browserPane fb =
        vLimitPercent 55 $ hLimitPercent width
        $ titledB browserFocused "Choose a file"
        $ renderFileBrowser browserFocused fb
      helpPane =
        let esact = if browserFocused
                    then if isDir then "change directory" else "load file"
                    else "save"
        in padTop (BC.Pad 1) $ hLimitPercent width $ vBox
           [ hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
           , hCenter $ txt $ "Enter/Space: " <> esact
           , hCenter $ txt "TAB: select Save/Load options"
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
      savePane = padTop (Pad 1)
                 $ vBox
                 [
                   let a = if fcsd == Just (WName "FMgr:SaveBtn")
                           then withAttr a'Selected
                           else if null f then withAttr a'Disabled else id
                       f = myProjFile ps
                       btxt = case f of
                                Nothing -> " "
                                Just x -> "[SAVE] " <> show x
                   in a $ str btxt
                 , if isDir
                   then withAttr a'Disabled $ str "[SAVE]"
                   else let a = if fcsd == Just (WName "FMgr:SaveAsBtn")
                                then withAttr a'Selected
                                else id
                            btxt =
                              case fileBrowserCursor =<< fB ps of
                                Just fi -> "[SAVE] " <> fileInfoFilePath fi
                                Nothing -> "[SAVE]"
                        in a $ str btxt
                 ]
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
                case parseAbsFile $ fileInfoFilePath f of
                  Just fp -> if unsavedChanges ts
                             then let msg = ConfirmLoad fp
                                  in return $ ts
                                     & projsChangedL .~ Left msg
                                     & fBrowser .~ Nothing
                             else (fBrowser .~ Nothing)
                                  <$> fileMgrReadProjectsFile fp ts
                  Nothing ->
                    -- Current selection is a directory; cannot load a directory,
                    -- so just allow the fBrowser to change to that directory.
                    return $ ts & fBrowser .~ Just b
      case ev of
        Vty.EvKey Vty.KEnter [] -> selectFile
        Vty.EvKey (Vty.KChar ' ') [] -> selectFile -- override filebrowser's default multi-select ability
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
    doSave f = case parseAbsFile $ fileInfoFilePath f of
                 Just fp -> fileMgrSaveProjectsFile fp ts
                 Nothing -> return $ ts & errMsgL .~ dirErr
    dirErr = "Cannot save to a directory: please select a file"


instance ToJSON ProjectName where toJSON (ProjectName pnm) = toJSON pnm
instance ToJSON LocationSpec where
  toJSON = genericToJSON locationSpecOptions
instance ToJSON Projects
instance ToJSON Project
instance ToJSON Group
instance ToJSON Role
instance ToJSON Language
instance ToJSON Location
instance ToJSON Note

instance FromJSON ProjectName where parseJSON = fmap ProjectName . parseJSON
instance FromJSON LocationSpec where
  parseJSON = genericParseJSON locationSpecOptions
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


locationSpecOptions :: Options
locationSpecOptions = defaultOptions { sumEncoding = UntaggedValue }


ensureDefaultProjectFile :: IO (Path Abs File)
ensureDefaultProjectFile = do
  dataDir <- getXdgDir XdgData $ Just [reldir|mywork|]
  createDirIfMissing True dataDir
  let pFile = dataDir </> [relfile|projects.json|]
  e <- doesFileExist pFile
  unless e $ BS.writeFile (toFilePath pFile) ""
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


readProjectsFile :: MonadIO m => Path Abs File -> m (Either String Projects)
readProjectsFile fp = do
  newprjs <- eitherDecode <$> liftIO (BS.readFile $ toFilePath fp)
  case newprjs of
    Right prjs -> Right . Projects <$> mapM syncProject (projects prjs)
    e@(Left _) -> return e

fileMgrReadProjectsFile :: MonadIO m
                        => Path Abs File
                        -> PaneState FileMgrPane MyWorkEvent
                        -> m (PaneState FileMgrPane MyWorkEvent)
fileMgrReadProjectsFile fp ps = readProjectsFile fp >>= \case
  Left er -> return $ ps & errMsgL .~ er
  Right prjs -> return $ ps
                & myProjectsL .~ prjs
                & myProjFileL .~ Just fp
                & projsChangedL .~ Right True
                & unsavedChangesL .~ False
                & exitMsgsL <>~ [ withAttr a'Notice $ str $ "Loaded " <> show fp ]


fileMgrSaveProjectsFile :: MonadIO m
                        => Path Abs File
                        -> PaneState FileMgrPane MyWorkEvent
                        -> m (PaneState FileMgrPane MyWorkEvent)
fileMgrSaveProjectsFile fp ps =
  do liftIO $ BS.writeFile (toFilePath fp) (encode $ myProjects ps)
     return $ ps
       & fBrowser .~ Nothing
       & myProjFileL .~ Just fp
       & unsavedChangesL .~ False
       & exitMsgsL <>~ [ withAttr a'Notice $ str $ "Wrote " <> show fp ]

-- | Called to display the FileMgr modal pane to allow the user to Load or Save.
showFileMgr :: PaneState FileMgrPane MyWorkEvent
            -> IO (PaneState FileMgrPane MyWorkEvent)
showFileMgr prev =
  case prev ^. fBrowser of
    Just _ -> return prev
    Nothing -> do
      let n = WName "FMgr:Browser"
      dataDir <- parent <$> ensureDefaultProjectFile
      fb <- newFileBrowser selectNonDirectories n (Just $ toFilePath dataDir)
      return $ prev & fBrowser .~ Just fb
