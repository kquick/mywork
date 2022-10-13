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
import qualified Brick.Widgets.Center as C
import           Control.Lens
import           Data.Maybe ( isJust )
import qualified Data.Sequence as Seq
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day )
import qualified Graphics.Vty as Vty

import           Defs
import           Panes.Common.Attrs
import           Panes.Common.Inputs


data LocationInputPane


data NewLoc = NewLoc { _nlName :: LocationSpec
                     , _nlDay :: Maybe Day
                     }

makeLenses ''NewLoc


blankNewLoc :: NewLoc
blankNewLoc = NewLoc (RemoteSpec "") Nothing

type LocForm = Form NewLoc MyWorkEvent WName

instance Pane WName MyWorkEvent LocationInputPane where
  data (PaneState LocationInputPane MyWorkEvent) = NL { nLF :: Maybe LocForm
                                                        -- Just == pane active
                                                      , nLoc :: Maybe Location
                                                      -- reset to Nothing when
                                                      -- nLF transitions Nothing
                                                      -- to Just
                                                      , nProj :: ProjectName
                                                      , nOrig :: Maybe Location
                                                      , nErr :: Maybe Text
                                                      }

  type (EventType LocationInputPane WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  initPaneState _ = NL Nothing Nothing (ProjectName "") Nothing Nothing
  drawPane ps _gs =
    C.centerLayer
    . modalB ((maybe "New" (const "Edit") $ nOrig ps)
              <> " " <> (let ProjectName pnm = nProj ps in T.pack $ show pnm)
              <> " Location")
    . vLimit 25
    . hLimitPercent 65
    . (\f -> vBox [ renderForm f
                  , padBottom (Pad 1) $ withAttr a'Error
                    $ maybe (txt " ") txt (nErr ps)
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
                             , locCore = LocRT
                                         { locValid = True  -- assumed
                                         }
                            }
      in if maybe False allFieldsValid pf
         then do let l = np . formState <$> pf
                 return $ s & nLFL .~ Nothing & newLocation .~ l
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
                     -> (Maybe LocationSpec, Maybe Location)
locationInputResults ps = (view locationL <$> nOrig ps, nLoc ps)


validateForm :: EventM WName es (PaneState LocationInputPane MyWorkEvent)
             -> EventM WName es (PaneState LocationInputPane MyWorkEvent)
validateForm inner = do
  s <- inner
  case s ^. nLFL of
    Nothing -> return s
    Just pf ->
      do (tgt,valid) <- validateLocationInput False $ formState pf ^. nlName
         return $ s & nLFL %~ fmap (setFieldValid valid tgt)


initLocInput :: ProjectName
             -> [Location]
             -> Maybe Location
             -> PaneState LocationInputPane MyWorkEvent
             -> PaneState LocationInputPane MyWorkEvent
initLocInput prjName locs mbLoc ps =
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
              label "Location" @@= locationInput locs mbLoc False nlName
            , label "Date" @@= mbDateInput nlDay
            ]
            (case mbLoc of
               Nothing -> blankNewLoc
               Just l -> NewLoc { _nlName = l ^. locationL
                                , _nlDay = l ^. locatedOnL
                                }
            )
      in NL { nLF = Just nlForm
            , nProj = prjName
            , nLoc = Nothing
            , nOrig = mbLoc
            , nErr = Nothing
            }
