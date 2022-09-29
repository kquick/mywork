{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sync
  -- (
  -- )
where

-- KWQ: location should be Path, not Text

import           Control.Applicative ( (<|>) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day )
import           Data.Time.Clock ( utctDay )
import           System.Directory ( doesDirectoryExist, getModificationTime )

import           Defs


data LocationStatus = LocationStatus { locExists :: Maybe Bool
                                     , lastUpd :: Maybe Day
                                     }


syncLocation :: MonadIO m => Location -> m LocationStatus
syncLocation l =
  if isLocationLocal l
  then do let LocationSpec lt = location l
          e <- liftIO $ doesDirectoryExist (T.unpack lt)
          u <- if e
               then Just . utctDay
                    <$> liftIO (getModificationTime (T.unpack lt))
               else return Nothing
          return $ LocationStatus { locExists = Just e, lastUpd = u }
  else return LocationStatus { locExists = Nothing, lastUpd = Nothing }


applyLocSync :: LocationStatus -> Location -> Location
applyLocSync locsts loc =
  loc { locValid = maybe True id $ locExists locsts
      , locatedOn = lastUpd locsts <|> locatedOn loc
      }


syncProject :: MonadIO m => Project -> m Project
syncProject p =
  let syncLoc l = flip applyLocSync l <$> syncLocation l
  in do locs <- mapM syncLoc (locations p)
        return $ p { locations = locs }
