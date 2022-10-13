{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Panes.ProjInfo
  (
    ProjInfoPane
  )
where

import           Brick
import           Brick.Panes
import           Control.Lens
import qualified Data.List as DL

import           Defs


data ProjInfoPane

instance Pane WName MyWorkEvent ProjInfoPane where
  data (PaneState ProjInfoPane MyWorkEvent) = Unused
  type (DrawConstraints ProjInfoPane s WName) = ( HasSelection s
                                                , HasProjects s
                                                )
  initPaneState _ = Unused
  drawPane _ gs =
    case selectedProject gs of
      Nothing -> Nothing
      Just p ->
        let mkPane prj =
              vBox
              [ withAttr a'ProjName (let ProjectName nm = p in txt nm)
                <+> vLimit 1 (fill ' ')
                <+> str "  ["
                <+> (str $ show $ prj ^. groupL)
                <+> str ", "
                <+> (withAttr (roleAttr $ prj ^. roleL)
                     $ str $ show $ prj ^. roleL)
                <+> str "]"
              , str "    " <+> txtWrap (prj ^. descriptionL)
              , str " "
              , txt $ "Language: " <> languageText (prj ^. languageL)
              ]
            nameMatch = (== p) . view projNameL
        in mkPane <$> DL.find nameMatch (projects $ snd $ getProjects gs)
