{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Panes.ProjInfo
  (
    ProjInfoPane
  )
where

import           Brick
import           Brick.Panes
import qualified Data.List as DL

import           Defs


data ProjInfoPane

instance Pane WName MyWorkEvent ProjInfoPane () where
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
              [ withAttr a'ProjName (txt p) <+> str "  ["
                <+> (withAttr (roleAttr $ role prj) $ str $ show $ role prj)
                <+> str "]"
              , str "    " <+> txtWrap (description prj)
              , str " "
              , txt $ "Language: " <> languageText (language prj)
              ]
        in mkPane <$> DL.find ((== p) . name) (projects $ snd $ getProjects gs)
