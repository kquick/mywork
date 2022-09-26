{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Defs where

import           Brick hiding (Location)
import           Brick.Focus
import           Brick.Panes
import           Control.Lens
import qualified Data.List as DL
import           Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import           Data.Time.Calendar
import           GHC.Generics ( Generic )


newtype Projects = Projects { projects :: [Project] }
  deriving (Generic, Monoid, Semigroup)

data Project = Project { name :: Text
                       , group :: Group
                       , role :: Role
                       , description :: Text
                       , language :: Either Text Language
                       , locations :: [Location]
                       }
  deriving Generic

data Group = Personal | Work | OtherGroup Text
  deriving (Eq, Generic)

data Role = Author | Maintainer | Contributor | User
  deriving (Show, Enum, Bounded, Eq, Generic)

data Language = Haskell | Rust | C | CPlusPlus | Python | JavaScript
  deriving (Show, Eq, Generic)

data Location = Location { location :: Text
                         , locatedOn :: Maybe Day
                         , locValid :: Bool
                         , notes :: [Note]
                         }
  deriving Generic

data Note = Note { note :: Text
                 , notedOn :: Day
                 }
  deriving Generic


numProjects :: Projects -> Int
numProjects = length . projects

languageText :: Either Text Language -> Text
languageText = either id (pack . show)


instance Show Group where
  show = \case
    Personal -> "Personal"
    Work -> "Work"
    OtherGroup g -> unpack g

noteTitle :: Note -> Text
noteTitle n = case T.lines $ note n of
                [] -> ""
                (t:_) -> t


----------------------------------------------------------------------

data MyWorkCore = MyWorkCore { myWorkFocus :: FocusRing WName }

initMyWorkCore :: MyWorkCore
initMyWorkCore = MyWorkCore { myWorkFocus = focusRing [ WProjList
                                                      , WLocation
                                                      ]
                            }

coreWorkFocusL :: Lens' MyWorkCore (FocusRing WName)
coreWorkFocusL f c = (\f' -> c { myWorkFocus = f' }) <$> f (myWorkFocus c)


data WName = WProjList | WLocation | WNotes | WName Text
  deriving (Eq, Ord)

instance Show WName where
  show = \case
    WProjList -> "Projects"
    WLocation -> "Location"
    WNotes -> "Notes"
    WName n -> unpack n


type MyWorkEvent = ()  -- No app-specific event for this simple app


class HasProjects s where
  getProjects :: s -> (Either Confirm Bool, Projects)


instance HasFocus MyWorkCore WName where
  getFocus f s =
    let setFocus jn = case focused jn of
          Nothing -> s
          Just n -> s & coreWorkFocusL %~ focusSetCurrent n
    in setFocus <$> (f $ Focused $ focusGetCurrent (s^.coreWorkFocusL))


class HasSelection s where
  selectedProject :: s -> Maybe Text

instance ( PanelOps Projects WName MyWorkEvent panes MyWorkCore
         , HasSelection (PaneState Projects MyWorkEvent)
         )
  => HasSelection (Panel WName MyWorkEvent MyWorkCore panes) where
  selectedProject = selectedProject . view (onPane @Projects)

class HasLocation s where
  selectedLocation :: s -> Maybe Text

instance ( PanelOps Location WName MyWorkEvent panes MyWorkCore
         , HasLocation (PaneState Location MyWorkEvent)
         )
  => HasLocation (Panel WName MyWorkEvent MyWorkCore panes) where
  selectedLocation = selectedLocation . view (onPane @Location)

class HasNote s where
  selectedNote :: s -> Maybe Text

instance ( PanelOps Note WName MyWorkEvent panes MyWorkCore
         , HasNote (PaneState Note MyWorkEvent)
         )
  => HasNote (Panel WName MyWorkEvent MyWorkCore panes) where
  selectedNote = selectedNote . view (onPane @Note)


getCurrentLocation :: HasSelection s
                   => HasLocation s
                   => HasProjects s
                   => s -> Maybe (Project, Maybe Location)
getCurrentLocation s = do p <- selectedProject s
                          let (_,prjs) = getProjects s
                          prj <- DL.find ((== p) . name) (projects prjs)
                          return  (prj
                                 , do l <- selectedLocation s
                                      DL.find ((== l) . location) (locations prj))


getCurrentNote :: HasNote s => s -> Location -> Maybe Note
getCurrentNote s l = do n <- selectedNote s
                        DL.find ((== n) . noteTitle) (notes l)

isLocationLocal :: Location -> Bool
isLocationLocal = isLocationLocal' . location

isLocationLocal' :: Text -> Bool
isLocationLocal' l = not $ or [ "http://" `T.isPrefixOf` l
                              , "https://" `T.isPrefixOf` l
                              , "git@" `T.isPrefixOf` l
                              ]

updateProject :: Maybe Text -> Project -> Projects -> Projects
updateProject onm p (Projects ps) =
  let oldName = maybe (name p) id onm
      (match, other) = DL.partition ((== oldName) . name) ps
      p' = foldr (updateLocation Nothing) p (concatMap locations match)
  in Projects $ p' : other


updateLocation :: Maybe Text -> Location -> Project -> Project
updateLocation ol l p =
  let oldName = maybe (location l) id ol
      (match, other) = DL.partition ((== oldName) . location) (locations p)
      l' = foldr addNote l (concatMap notes match)
      addNote n lc = lc { notes = n : filter ((/= noteTitle n) . noteTitle) (notes lc) }
  in p { locations = l' : other }


updateNote :: Maybe Text -> Note -> Location -> Project -> (Project, Location)
updateNote oldn n l p =
  let oldName = maybe (noteTitle n) id oldn
      newL = l { notes = n : filter ((/= oldName) . noteTitle) (notes l) }
  in (updateLocation Nothing newL p, newL)


data OpOn = ProjectOp | LocationOp | NoteOp
  deriving (Eq, Enum, Bounded)

opOnSelection :: HasSelection s
              => HasLocation s
              => HasFocus s WName
              => s -> (OpOn, Maybe Text)
opOnSelection s = case s ^. getFocus of
                    Focused (Just WProjList) -> (ProjectOp, selectedProject s)
                    Focused (Just WLocation) -> (LocationOp, selectedLocation s)
                    Focused (Just WNotes) -> (NoteOp, Nothing) -- TODO: selectedNote
                    _ -> (ProjectOp, Nothing)


data Confirm = ConfirmProjectDelete Text -- project name
             | ConfirmLocationDelete Text Text -- project name, location
             | ConfirmNoteDelete Text Text Text -- project name, location, noteTitle
             | ConfirmLoad String -- filepath
             | ConfirmQuit

-- The Show instance for Confirm is the message presented to the user in the
-- confirmation window.
instance Show Confirm where
  show = \case
    ConfirmProjectDelete pname ->
      "Are you sure you want to delete project " <> show pname
      <> " and all associated locations and notes?"
    ConfirmLocationDelete pname locn ->
      "Are you sure you want to remove location " <> show locn
      <> " from project " <> show pname <> "?"
    ConfirmNoteDelete pname locn nt ->
      "Remove the following note from project " <> show pname
      <> ", location " <> show locn <> "?\n\n  " <> show nt
    ConfirmLoad fp ->
      "Discard local changes and load projects from " <> show fp <> "?"
    ConfirmQuit -> "There are unsaved changes.  Are you sure you want to quit?"


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
