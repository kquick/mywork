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
import           Control.Applicative ( (<|>) )
import           Control.Lens
import           Control.Monad ( guard )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.List as DL
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day, fromGregorianValid )
import           Path ( parseAbsDir )
import           Path.IO ( doesDirExist, doesDirExist )
import           Text.Read ( readMaybe )

import           Defs



headText :: [Text] -> Text
headText = \case
  [] -> ""
  (o:_) -> o


textToDay :: Text -> Maybe Day
textToDay t =
  case T.split (`T.elem` "-/.") t of
    [y,m,d] ->
      let validYear x = if x < (1800 :: Integer) then x + 2000 else x
          validMonth x = not (x < 1 || x > (12 :: Int))
          validDayOfMonth x = not (x < 1 || x > (31 :: Int))
          months = [ "january", "february", "march", "april"
                   , "may", "june", "july", "august"
                   , "september", "october", "november", "december"
                   ]
          ml = T.toLower m
          matchesMonth x = or [ ml == x, ml == T.take 3 x]
      in do y' <- validYear <$> readMaybe (T.unpack y)
            m' <- readMaybe (T.unpack m)
                  <|> (snd <$> (DL.find (matchesMonth . fst) $ zip months [1..]))
            guard (validMonth m')
            d' <- readMaybe (T.unpack d)
            guard (validDayOfMonth d')
            fromGregorianValid y' m' d'
    _ -> Nothing


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
