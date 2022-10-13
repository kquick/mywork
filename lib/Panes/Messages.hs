{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.Messages
  (
    MessagesPane
  )
where

import           Brick
import           Brick.Panes
import           Control.Lens

import           Defs


data MessagesPane

instance Pane WName MyWorkEvent MessagesPane where
  data (PaneState MessagesPane MyWorkEvent) = M [Widget WName]
  type UpdateType MessagesPane = Maybe [Widget WName]
  initPaneState _ = M mempty
  drawPane (M msgs) _ = Just $ if null msgs then str " " else vBox msgs
  updatePane mbw (M msgs) = case mbw of
                              Nothing -> M mempty
                              Just ms -> M $ msgs <> ms


instance HasMessage (PaneState MessagesPane MyWorkEvent) where
  getMessage (M msgs) = msgs

instance ( PanelOps MessagesPane WName MyWorkEvent panes MyWorkCore
         , HasMessage (PaneState MessagesPane MyWorkEvent)
         )
  => HasMessage (Panel WName MyWorkEvent MyWorkCore panes) where
  getMessage = getMessage . view (onPane @MessagesPane)
