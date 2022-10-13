{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Panes.Common.Inputs
  -- (
  -- )
where

import           Brick hiding ( Location )
import           Brick.Forms
import           Control.Lens
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day )
import           Path ( parseAbsDir )
import           Path.IO ( doesDirExist, doesDirExist )

import           Defs



headText :: [Text] -> Text
headText = \case
  [] -> ""
  (o:_) -> o


locationInput :: [Location]
              -> Maybe Location
              -> Bool
              -> Lens' s LocationSpec
              -> s
              -> FormFieldState s e WName
locationInput locs mbLoc blankAllowed stateLens =
  let validate = \case
        [] -> if blankAllowed
              then Just (RemoteSpec "")
              else Nothing
        (l:_) ->
          let lr = RemoteSpec l
              ll = parseAbsDir $ T.unpack l
              ls = T.unpack l
          in if or
                [
                  -- Should not match any existing location
                  and [ ls `elem` (show . view locationL <$> locs)
                      , maybe True ((ls /=) . show . view locationL) mbLoc
                      ]

                  -- Check blank v.s. allowed and should be an absolute path if
                  -- it looks like a local path
                , and [ not blankAllowed
                      , maybe (isLocationTextLocal (T.pack ls)) (const False) ll
                      ]
                ]
             then Nothing  -- invalid
             else Just $ maybe lr LocalSpec ll
  in editField stateLens (WName "New Location") (Just 1)
     (T.pack . show) validate (txt . headText) id


validateLocationInput :: MonadIO m => Bool -> LocationSpec -> m (WName, Bool)
validateLocationInput blankAllowed l =
  let tgt = WName "New Location"
      ls = T.pack $ show l
  in if blankAllowed && T.null ls
     then return (tgt, True)
     else case l of
            LocalSpec lp -> (tgt,) <$> (liftIO $ doesDirExist lp)
            RemoteSpec _ -> return (tgt, True)


mbDateInput :: Lens' s (Maybe Day)
            -> s
            -> FormFieldState s e WName
mbDateInput stateLens =
  let validate = \case
        ("":_) -> Just Nothing
        (l:_) -> Just <$> textToDay l
        _ -> Nothing
      dayInit = maybe "" (T.pack . show)
      dayRender = txt . headText
  in editField stateLens (WName "Location Date (Y-M-D)")
     (Just 1) dayInit validate dayRender id
