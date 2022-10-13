{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Defs.JSON
where

import Control.Lens
import Data.Aeson

import Defs


instance ToJSON ProjectName where toJSON (ProjectName pnm) = toJSON pnm
instance ToJSON LocationSpec where
  toJSON = genericToJSON locationSpecOptions
instance ToJSON Projects
instance ToJSON Project
instance ToJSON Group
instance ToJSON Role
instance ToJSON Language
instance ToJSON Location where
  -- only emit notes with NoteSource of MyWorkDB
  toJSON l = object [ ("location", toJSON (l ^. locationL))
                    , ("locatedOn", toJSON (l ^. locatedOnL))
                    , ("locValid", toJSON (l ^. locValidL))
                    , ("notes",
                       toJSON (filter ((MyWorkDB ==) . view noteSourceL) $ l^.notesL))
                    ]
instance ToJSON Note where
  -- does not emit noteSource
  toJSON n = object [ ("note", toJSON (n ^. noteL))
                    , ("notedOn", toJSON (n ^. notedOnL))
                    ]

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
instance FromJSON Note where
  parseJSON = withObject "Note" $ \v -> Note
    <$> v .: "notedOn"
    <*> v .: "note"
    <*> pure MyWorkDB


locationSpecOptions :: Options
locationSpecOptions = defaultOptions { sumEncoding = UntaggedValue }
