{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sync
  -- (
  -- )
where

import           Control.Applicative ( (<|>) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Time.Calendar ( Day )
import           Data.Time.Clock ( utctDay )
import           Path.IO ( doesDirExist, getModificationTime )

import           Defs


data LocationStatus = LocationStatus { locExists :: Maybe Bool
                                     , lastUpd :: Maybe Day
                                     }


syncLocation :: MonadIO m => Location -> m LocationStatus
syncLocation l = case location l of
  LocalSpec lcl ->
    do e <- liftIO $ doesDirExist lcl
       u <- if e
            then Just . utctDay <$> liftIO (getModificationTime lcl)
            else return Nothing
       return $ LocationStatus { locExists = Just e, lastUpd = u }
  RemoteSpec _ -> return LocationStatus { locExists = Nothing
                                        , lastUpd = Nothing
                                        }


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
