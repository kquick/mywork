{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Defs.JSON () where

import Control.Applicative ( (<|>) )
import Data.Aeson

import Defs
import Defs.Static ()


instance ToJSON ProjectName where toJSON (ProjectName pnm) = toJSON pnm
instance ToJSON LocationSpec where
  toJSON = genericToJSON locationSpecOptions
instance ToJSON (Projects_ ())
instance ToJSON (Project_ ()) where
  toJSON = genericToJSON projectOptions
instance ToJSON Group
instance ToJSON Role
instance ToJSON Language
instance ToJSON (Location_ ()) where
  toJSON = genericToJSON locationOptions
instance ToJSON (Note_ ()) where
  toJSON = genericToJSON noteOptions

instance FromJSON ProjectName where parseJSON = fmap ProjectName . parseJSON
instance FromJSON LocationSpec where
  parseJSON = genericParseJSON locationSpecOptions
instance FromJSON (Projects_ ())
instance FromJSON (Project_ ()) where
  parseJSON = withObject "Project" $ \v -> Project
    <$> (v .: "name" <|> v .: "projName")
    <*> v .:? "group" .!= Personal
    <*> v .: "role"
    <*> v .: "description"
    <*> v .: "language"
    <*> v .: "locations"
    <*> pure Nothing

instance FromJSON Group
instance FromJSON Role
instance FromJSON Language
instance FromJSON (Location_ ()) where
  parseJSON = withObject "Location" $ \v -> Location
    <$> v .: "location"
    <*> v .:? "locatedOn" .!= Nothing
    <*> v .:? "locValid" .!= True -- assumed -- Added in v0.1.1.0
    <*> v .: "notes"
    <*> pure Nothing
instance FromJSON (Note_ ())


projectOptions :: Options
projectOptions = defaultOptions { omitNothingFields = True }

locationOptions :: Options
locationOptions = defaultOptions { omitNothingFields = True }

locationSpecOptions :: Options
locationSpecOptions = defaultOptions { sumEncoding = UntaggedValue }

noteOptions :: Options
noteOptions = defaultOptions { omitNothingFields = True }
