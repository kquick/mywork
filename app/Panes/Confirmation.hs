{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.Confirmation
  (
    isConfirmationActive
  , getConfirmedAction
  , showConfirmation
  )
where

import           Brick
import           Brick.Panes
import           Brick.Widgets.Dialog
import           Control.Lens
import           Data.Maybe ( isJust )
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty

import           Defs


instance Pane WName MyWorkEvent Confirm (Maybe Confirm) where
  data (PaneState Confirm MyWorkEvent) = Cf { cD :: Maybe (Dialog Bool)
                                            , cW :: Maybe Confirm
                                            }
  type (DrawConstraints Confirm s WName) = ( HasFocus s WName )
  initPaneState _ = Cf Nothing Nothing
  drawPane ps _ =
    let draw d = renderDialog d ( padBottom (Pad 1)
                                  $ padLeft (Pad 1)
                                  $ padRight (Pad 1)
                                  $ strWrap $ maybe "Really?" show $ cW ps)
    in draw <$> cD ps
  focusable _ ps = case cD ps of
                     Nothing -> mempty
                     Just _ -> Seq.fromList [ WName "Confirmation" ]
  handlePaneEvent _ = \case
    Vty.EvKey Vty.KEsc [] -> \_ -> return $ Cf Nothing Nothing
    Vty.EvKey Vty.KEnter [] -> \ps -> case cD ps of
      Nothing -> return ps  -- shouldn't happen
      Just d -> return $ ps { cD = Nothing
                            , cW = case dialogSelection d of
                                     Just True -> cW ps
                                     _ -> Nothing
                            }
    ev -> cDL . _Just %%~ \w -> nestEventM' w (handleDialogEvent ev)
  updatePane mbCnf ps = ps { cW = mbCnf }


cDL :: Lens' (PaneState Confirm MyWorkEvent) (Maybe (Dialog Bool))
cDL = lens cD (\s v -> s { cD = v })


-- | Returns true if the Confirmation modal is active and being displayed.  This
-- is primarily used for detecting the transition from displayed -> not displayed
-- which indicates the user made a decision.
isConfirmationActive :: PaneState Confirm MyWorkEvent -> Bool
isConfirmationActive = isJust . cD


-- | Retrieve the confirmation, if there is one.
--
-- Verifies that the state is valid for providing confirmation.  The confirmation
-- status is cleared by this action in the returned pane state.
getConfirmedAction :: PaneState Confirm MyWorkEvent
                   -> (PaneState Confirm MyWorkEvent, Maybe Confirm)
getConfirmedAction ps = do
  case (cD ps, cW ps) of
    (Nothing, Just cnf) -> (ps { cW = Nothing }, Just cnf)
    _ -> (ps, Nothing)


-- | Activate the confirmation modal with the provided Confirm message and
-- action.
showConfirmation :: Confirm
                 -> PaneState Confirm MyWorkEvent
                 -> PaneState Confirm MyWorkEvent
showConfirmation for _ =
  let d = dialog Nothing -- (Just $ show for)
          (Just (1, [ ("OK", True), ("Cancel", False) ]))
          70  -- max width of dialog
  in Cf (Just d) (Just for)
