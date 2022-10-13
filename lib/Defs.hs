{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Defs where

import           Brick hiding (Location)
import           Brick.Focus
import           Brick.Panes
import           Control.Lens
import           Control.Monad ( guard )
import qualified Data.List as DL
import           Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import           Data.Time.Calendar
import           GHC.Generics ( Generic )
import           Path ( Path, Abs, Dir, File, toFilePath )

import Defs.Lenses


type family ProjectCore a
type family LocationCore a
type family NoteCore a

newtype Projects_ core = Projects { projects :: [Project_ core] }
  deriving (Generic, Monoid, Semigroup)

newtype ProjectName = ProjectName Text deriving (Eq, Ord)

data Project_ core =
  Project { projName :: ProjectName
          , group :: Group
          , role :: Role
          , description :: Text
          , language :: Either Text Language
          , locations :: [Location_ core]
          , projCore :: ProjectCore core
          }
  deriving Generic

data Group = Personal | Work | OtherGroup Text
  deriving (Eq, Generic)

data Role = Author | Maintainer | Contributor | User
  deriving (Show, Enum, Bounded, Eq, Ord, Generic)

data Language = Haskell | Rust | C | CPlusPlus | Python | JavaScript | Prolog
  deriving (Show, Eq, Generic)

data LocationSpec = LocalSpec (Path Abs Dir)
                  | RemoteSpec Text
                  deriving (Eq, Ord, Generic)

instance Show LocationSpec where
  show = \case
    LocalSpec p -> toFilePath p
    RemoteSpec r -> T.unpack r

data Location_ core = Location { location :: LocationSpec
                               , locatedOn :: Maybe Day
                               , locValid :: Bool
                               , notes :: [Note_ core]
                               , locCore :: LocationCore core
                               }
  deriving Generic

data Note_ core = Note { notedOn :: Day
                       , note :: Text
                       , noteSource :: NoteSource
                       , noteCore :: NoteCore core
                       }
  deriving (Generic)

instance Eq (Note_ core) where
  n1 == n2 = noteTitle n1 == noteTitle n2 && notedOn n1 == notedOn n2
instance Ord (Note_ core) where
  compare n1 n2 = case compare (noteTitle n1) (noteTitle n2) of
                    EQ -> compare (notedOn n1) (notedOn n2)
                    o -> o

data NoteSource = MyWorkDB | ProjLoc | MyWorkGenerated
  deriving (Eq, Ord)

newtype NoteTitle = NoteTitle Text deriving (Eq, Ord)

noteTitle :: Note_ core -> NoteTitle
noteTitle = noteTitle' . note

noteTitle' :: Text -> NoteTitle
noteTitle' t = case T.lines t of
                 [] -> NoteTitle ""
                 (l:_) -> NoteTitle l


----------------------------------------------------------------------

data Live

data ProjRT = ProjRT
data LocRT = LocRT
data NoteRT = NoteRT

type instance ProjectCore Live = ProjRT

type instance LocationCore Live = LocRT

type instance NoteCore Live = NoteRT

type Projects = Projects_ Live
type Project = Project_ Live
type Location = Location_ Live
type Note = Note_ Live

----------------------------------------------------------------------

makeLensL ''Project_
makeLensL ''Location_
makeLensL ''Note_

----------------------------------------------------------------------

numProjects :: Projects -> Int
numProjects = length . projects

languageText :: Either Text Language -> Text
languageText = either id (pack . show)


instance Show Group where
  show = \case
    Personal -> "Personal"
    Work -> "Work"
    OtherGroup g -> unpack g

----------------------------------------------------------------------

data MyWorkCore = MyWorkCore { myWorkFocus :: FocusRing WName }

initMyWorkCore :: MyWorkCore
initMyWorkCore = MyWorkCore { myWorkFocus = focusRing [ WProjList
                                                      , WLocations
                                                      ]
                            }

coreWorkFocusL :: Lens' MyWorkCore (FocusRing WName)
coreWorkFocusL f c = (\f' -> c { myWorkFocus = f' }) <$> f (myWorkFocus c)


data WName = WProjList | WLocations | WNotes | WName Text
  deriving (Eq, Ord)

instance Show WName where
  show = \case
    WProjList -> "Projects"
    WLocations -> "Location"
    WNotes -> "Notes"
    WName n -> unpack n


type MyWorkEvent = ()  -- No app-specific event for this simple app


class HasProjects s where
  getProjects :: s -> (Either Confirm Bool, Projects)


class HasMessage s where
  getMessage :: s -> [Widget WName]


instance HasFocus MyWorkCore WName where
  getFocus f s =
    let setFocus jn = case focused jn of
          Nothing -> s
          Just n -> s & coreWorkFocusL %~ focusSetCurrent n
    in setFocus <$> (f $ Focused $ focusGetCurrent (s^.coreWorkFocusL))


class HasSelection s where
  selectedProject :: s -> Maybe ProjectName

instance ( PanelOps Projects WName MyWorkEvent panes MyWorkCore
         , HasSelection (PaneState Projects MyWorkEvent)
         )
  => HasSelection (Panel WName MyWorkEvent MyWorkCore panes) where
  selectedProject = selectedProject . view (onPane @Projects)

class HasLocation s where
  -- | Returns the currently selected project and location
  selectedLocation :: s -> Maybe (ProjectName, LocationSpec)

instance ( PanelOps Location WName MyWorkEvent panes MyWorkCore
         , HasLocation (PaneState Location MyWorkEvent)
         )
  => HasLocation (Panel WName MyWorkEvent MyWorkCore panes) where
  selectedLocation = selectedLocation . view (onPane @Location)

class HasNote s where
  -- | Returns the currently selected location and note
  selectedNote :: s -> Maybe (LocationSpec, NoteTitle)

instance ( PanelOps Note WName MyWorkEvent panes MyWorkCore
         , HasNote (PaneState Note MyWorkEvent)
         )
  => HasNote (Panel WName MyWorkEvent MyWorkCore panes) where
  selectedNote = selectedNote . view (onPane @Note)


getCurrentProject :: HasSelection s => HasProjects s => s -> Maybe Project
getCurrentProject s = do pnm <- selectedProject s
                         let (_, prjs) = getProjects s
                         DL.find ((== pnm) . view projNameL) (projects prjs)

getCurrentLocation :: HasSelection s
                   => HasLocation s
                   => HasProjects s
                   => s -> Maybe (Project, Maybe Location)
getCurrentLocation s =
  do (p,l) <- selectedLocation s
     let (_,prjs) = getProjects s
     prj <- DL.find ((== p) . view projNameL) (projects prjs)
     return (prj, DL.find ((== l) . view locationL) (prj ^. locationsL))


getCurrentNote :: HasNote s => s -> Location -> Maybe Note
getCurrentNote s l = do (l',n) <- selectedNote s
                        guard (l ^. locationL == l')
                        DL.find ((== n) . noteTitle) (l ^. notesL)

isLocationLocal :: Location -> Bool
isLocationLocal = isLocationLocal' . view locationL

isLocationLocal' :: LocationSpec -> Bool
isLocationLocal' = \case
  LocalSpec _ -> True
  RemoteSpec _ -> False

isLocationTextLocal :: Text -> Bool
isLocationTextLocal t =
  not $ or [ "http://" `T.isPrefixOf` t
           , "https://" `T.isPrefixOf` t
           , "git@" `T.isPrefixOf` t
           , ":/" `T.isInfixOf` t
           ]

updateProject :: Maybe ProjectName -> Project -> Projects -> Projects
updateProject onm p (Projects ps) =
  let oldName = maybe (p ^. projNameL) id onm
      (match, other) = DL.partition ((== oldName) . view projNameL) ps
      p' = foldr (updateLocation Nothing) p (concatMap (view locationsL) match)
  in Projects $ p' : other


-- | Adds the specified Location to the Project, merging with the previous
-- Location (with the same name or the previous name indicated by a Just in the
-- the Maybe parameter).
updateLocation :: Maybe LocationSpec -> Location -> Project -> Project
updateLocation ol l p =
  let oldName = maybe (l ^. locationL) id ol
      isOldName = (oldName ==) . view locationL
      (match, other) = DL.partition isOldName (p ^. locationsL)
      l' = foldr addNote l (concatMap (view notesL) match)
      addNote n = notesL %~ (n :) . filter ((/= noteTitle n) . noteTitle)
  in p & locationsL .~ l' : other


updateLocNote :: Maybe NoteTitle -> Note -> Location -> Location
updateLocNote oldn n =
  let oldName = maybe (noteTitle n) id oldn
  in notesL %~ (n :) . filter ((/= oldName) . noteTitle)


updateNote :: Maybe NoteTitle -> Note -> Location -> Project
           -> (Project, Location)
updateNote oldn n l p = let newL = updateLocNote oldn n l
                        in (updateLocation Nothing newL p, newL)


data OpOn = ProjectOp | LocationOp | NoteOp
  deriving (Eq, Enum, Bounded)

opOnSelection :: HasSelection s
              => HasLocation s
              => HasFocus s WName
              => s -> OpOn
opOnSelection s =
  case s ^. getFocus of
    Focused (Just WProjList) -> ProjectOp
    Focused (Just WLocations) -> LocationOp
    Focused (Just WNotes) -> NoteOp
    _ -> ProjectOp


data Confirm = ConfirmProjectDelete ProjectName
             | ConfirmLocationDelete ProjectName LocationSpec
             | ConfirmNoteDelete ProjectName LocationSpec NoteTitle
             | ConfirmLoad (Path Abs File)
             | ConfirmQuit

-- The Show instance for Confirm is the message presented to the user in the
-- confirmation window.
instance Show Confirm where
  show = \case
    ConfirmProjectDelete pname ->
      let ProjectName pnm = pname
      in "Are you sure you want to delete project " <> show pnm
         <> " and all associated locations and notes?"
    ConfirmLocationDelete pname locn ->
      let ProjectName pnm = pname
      in "Are you sure you want to remove location " <> show locn
         <> " from project " <> show pnm <> "?"
    ConfirmNoteDelete pname locn nt ->
      let ProjectName pnm = pname
          NoteTitle ntitle = nt
      in "Remove the following note from project " <> show pnm
         <> ", location " <> show locn <> "?\n\n  " <> show ntitle
    ConfirmLoad fp ->
      "Discard local changes and load projects from " <> show fp <> "?"
    ConfirmQuit -> "There are unsaved changes.  Are you sure you want to quit?"


tshow :: Show a => a -> Text
tshow = T.pack . show

----------------------------------------------------------------------

a'Operation :: AttrName
a'Operation = attrName "Oper"

a'RoleAuthor, a'RoleContributor, a'RoleMaintainer, a'RoleUser :: AttrName
a'RoleAuthor = attrName "auth"
a'RoleContributor = attrName "contrib"
a'RoleMaintainer = attrName "maint"
a'RoleUser = attrName "user"

roleAttr :: Role -> AttrName
roleAttr = \case
  Author -> a'RoleAuthor
  Contributor -> a'RoleContributor
  Maintainer -> a'RoleMaintainer
  User -> a'RoleUser


a'ProjName :: AttrName
a'ProjName = attrName "projname"

a'Disabled :: AttrName
a'Disabled = attrName "disabled"

a'Selected :: AttrName
a'Selected = attrName "selected"

a'Error :: AttrName
a'Error = attrName "Error"

a'Notice :: AttrName
a'Notice = attrName "Notice"
