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
import           Brick.Widgets.Border
import           Control.Lens
import qualified Data.List as DL
import           Data.Text ( Text, pack, unpack )
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

----------------------------------------------------------------------

data MyWorkCore = MyWorkCore { projFile :: FilePath
                             , myWorkFocus :: FocusRing WName
                             }

initMyWorkCore :: MyWorkCore
initMyWorkCore = MyWorkCore { projFile = "projects.json"
                            , myWorkFocus = focusRing [ WProjList
                                                      , WLocation
                                                      ]
                            }

coreWorkFocusL :: Lens' MyWorkCore (FocusRing WName)
coreWorkFocusL f c = (\f' -> c { myWorkFocus = f' }) <$> f (myWorkFocus c)


data WName = WProjList | WLocation | WName Text
  deriving (Eq, Ord)

instance Show WName where
  show = \case
    WProjList -> "Projects"
    WLocation -> "Location"
    WName n -> unpack n


-- | Adds a border with a title to the current widget.  First argument is True if
-- the current widget has focus.
titledB :: Bool -> Text -> Widget WName -> Widget WName
titledB fcsd text =
  let ttlAttr = if fcsd then withAttr (attrName "Selected") else id
  in borderWithLabel (ttlAttr $ txt text)


type MyWorkEvent = ()  -- No app-specific event for this simple app


class HasProjects s where
  getProjects :: s -> (Bool, Projects)


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


updateProject :: Project -> Projects -> Projects
updateProject p (Projects ps) = Projects $ p : filter ((/= name p) . name) ps


----------------------------------------------------------------------

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
