{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Panes.LocationInput
  (
    LocationInputPane
  , initLocInput
  , isLocInputActive
  , locationInputResults
  )
where

import           Brick hiding ( Location )
import           Brick.Focus
import           Brick.Forms
import           Brick.Panes
import           Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import           Control.Applicative ( (<|>) )
import           Control.Lens hiding ( under )
import           Control.Monad ( guard )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.List as DL
import           Data.Maybe ( isJust )
import qualified Data.Sequence as Seq
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day )
import           Data.Time.Calendar ( fromGregorianValid )
import qualified Graphics.Vty as Vty
import           System.Directory ( doesDirectoryExist )
import           System.FilePath ( isValid, isRelative, normalise )
import           Text.Read

import           Defs


data LocationInputPane


data NewLoc = NewLoc { _nlName :: Text
                     , _nlDay :: Maybe Day
                     }

makeLenses ''NewLoc


blankNewLoc :: NewLoc
blankNewLoc = NewLoc "" Nothing

type LocForm = Form NewLoc MyWorkEvent WName

instance Pane WName MyWorkEvent LocationInputPane () where
  data (PaneState LocationInputPane MyWorkEvent) = NL { nLF :: Maybe LocForm
                                                        -- Just == pane active
                                                      , nLoc :: Maybe Location
                                                      -- reset to Nothing when
                                                      -- nLF transitions Nothing
                                                      -- to Just
                                                      , nProj :: Text
                                                      , nOrig :: Maybe Location
                                                      , nErr :: Maybe Text
                                                }
  type (EventType LocationInputPane WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  initPaneState _ = NL Nothing Nothing "" Nothing Nothing
  drawPane ps _gs =
    C.centerLayer
    . borderWithLabel (str $ (maybe "New" (const "Edit") $ nOrig ps)
                             <> " " <> show (nProj ps) <> " Location")
    . vLimit 25
    . hLimitPercent 65
    . (\f -> vBox [ renderForm f
                  , padBottom (Pad 1) $ withAttr a'Error
                    $ maybe emptyWidget txt (nErr ps)
                  , emptyWidget
                  , vLimit 1 (fill ' ' <+> str "Ctrl-D = accept"
                              <+> fill ' ' <+> str "ESC = abort"
                              <+> fill ' ')
                  ]) <$> nLF ps
  focusable _ ps = case nLF ps of
                     Nothing -> mempty
                     Just f -> Seq.fromList $ focusRingToList $ formFocus f
  handlePaneEvent _ = \case
    VtyEvent (Vty.EvKey Vty.KEsc []) -> nLFL %%~ const (return Nothing)
    VtyEvent (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) -> \s ->
      let pf = s ^. nLFL
          np form = Location { location = form ^. nlName
                             , locatedOn = form ^. nlDay
                             , notes = mempty
                            }
      in if maybe False allFieldsValid pf
         then
           return $ s & nLFL .~ Nothing & newLocation .~ (np . formState <$> pf)
         else
           let badflds = maybe "none"
                         (foldr (\n a -> if T.null a
                                         then T.pack n
                                         else T.pack n <> ", " <> a) ""
                          . fmap show . invalidFields)
                         pf
               errmsg = "Correct invalid entries before accepting: "
           in return $ s { nErr = Just $ errmsg <> badflds }
    ev -> \s -> validateForm
                $ s { nErr = Nothing }
                & (nLFL . _Just %%~ \w -> nestEventM' w (handleFormEvent ev))


nLFL :: Lens' (PaneState LocationInputPane MyWorkEvent) (Maybe LocForm)
nLFL f s = (\n -> s { nLF = n }) <$> f (nLF s)

isLocInputActive :: PaneState LocationInputPane MyWorkEvent -> Bool
isLocInputActive = isJust . nLF


newLocation :: Lens' (PaneState LocationInputPane MyWorkEvent) (Maybe Location)
newLocation f s = (\n -> s { nLoc = n }) <$> f (nLoc s)


-- | Returns the original location name (if any) and the new Location
-- specification.
locationInputResults :: PaneState LocationInputPane MyWorkEvent
                     -> (Maybe Text, Maybe Location)
locationInputResults ps = (location <$> nOrig ps, nLoc ps)


inpToDay :: Text -> Maybe Day
inpToDay t = let t' = T.split (`T.elem` "-/") t
             in case t' of
                  [y,m,d] ->
                    let validYear x = if x < (1800 :: Integer)
                                      then x + 2000
                                      else x
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
                                <|> (snd <$> (DL.find (matchesMonth . fst)
                                              $ zip months [1..]))
                          guard (validMonth m')
                          d' <- readMaybe (T.unpack d)
                          guard (validDayOfMonth d')
                          fromGregorianValid y' m' d'
                  _ -> Nothing


validateForm :: EventM WName es (PaneState LocationInputPane MyWorkEvent)
             -> EventM WName es (PaneState LocationInputPane MyWorkEvent)
validateForm inner = do
  s <- inner
  case s ^. nLFL of
    Nothing -> return s
    Just pf ->
      let l = formState pf ^. nlName
          tgt = WName "New Location"
      in if isLocationLocal' l
         then do e <- liftIO $ doesDirectoryExist $ T.unpack l
                 return $ s & nLFL %~ fmap (setFieldValid e tgt)
         else return s


initLocInput :: Text -- Project Name
             -> [Location]
             -> Maybe Location
             -> PaneState LocationInputPane MyWorkEvent
             -> PaneState LocationInputPane MyWorkEvent
initLocInput projName locs mbLoc ps =
  case nLF ps of
    Just _ -> ps
    Nothing ->
      let label s = padBottom (Pad 1) . label' s
          label' s w = (vLimit 1 $ hLimit labelWidth
                        $ fill ' ' <+> str s <+> str ": ") <+> w
          labelWidth = 15
          nlForm =
            newForm
            [
              label "Location" @@=
              let validate = \case
                    (l:_) -> if (l `elem` (location <$> locs)
                                  && (maybe True ((l /=) . location) mbLoc))
                                || not (isValid (T.unpack l))
                                || (isLocationLocal' l && isRelative (T.unpack l))
                              then Nothing  -- invalid
                              else Just $ T.pack $ normalise $ T.unpack l
                    o -> Just $ T.intercalate "\n" o
              in editField nlName (WName "New Location") (Just 1)
                 id validate (txt . head) id
            , label "Date" @@= let validate = \case
                                     ("":_) -> Just Nothing
                                     (l:_) -> Just <$> inpToDay l
                                     _ -> Nothing
                                   dayInit = maybe "" (T.pack . show)
                                   dayRender = txt . head
                               in editField nlDay (WName "Location Date (Y-M-D)")
                                  (Just 1) dayInit validate dayRender id
            ]
            (case mbLoc of
               Nothing -> blankNewLoc
               Just l -> NewLoc { _nlName = location l
                                , _nlDay = locatedOn l
                                }
            )
      in NL { nLF = Just nlForm
            , nProj = projName
            , nLoc = Nothing
            , nOrig = mbLoc
            , nErr = Nothing
            }
