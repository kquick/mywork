{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Draw
  (
    drawMyWork
  , myattrs
  )
where

import Brick hiding ( Location )
import Brick.Widgets.Edit
import Data.Maybe ( catMaybes )
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Dialog
import Brick.Forms
import Brick.Widgets.List
import Brick.Panes
import Data.Version ( showVersion )
import Graphics.Vty

import Defs
import Panes.AddProj
import Panes.FileMgr
import Panes.Help
import Panes.LocationInput
import Panes.Messages
import Panes.NoteInput
import Panes.Operations
import Panes.ProjInfo
import Panes.Summary
import Paths_mywork ( version )
import Whole


myattrs :: AttrMap
myattrs = attrMap defAttr
          [
            (editAttr, white `on` black)
          , (editFocusedAttr, yellow `on` black)

          , (listAttr, defAttr `withStyle` defaultStyleMask)
          , (listSelectedAttr, defAttr `withStyle` bold `withStyle` underline)
          , (listSelectedFocusedAttr, defAttr `withStyle` reverseVideo)

          , (invalidFormInputAttr, fg red `withStyle` bold)
          , (focusedFormInputAttr, defAttr `withStyle` reverseVideo)

          , (buttonAttr, black `on` (rgbColor 100 100 (100::Int)))
          , (buttonSelectedAttr, black `on` green)

          , (a'Operation, white `on` (rgbColor 0 1 (0::Int)))

          , (a'RoleAuthor, fg $ rgbColor 0 255 (0::Int))
          , (a'RoleMaintainer, fg $ rgbColor 0 200 (130::Int))
          , (a'RoleContributor, fg $ rgbColor 0 145 (180::Int))
          , (a'RoleUser, defAttr)

          , (a'ProjName, defAttr `withStyle` bold `withStyle` underline)

          , (a'NoteSourceMyWork, defAttr)
          , (a'NoteSourceProjLoc, defAttr)
          , (a'NoteSourceGenerated, white `on` black `withStyle` dim)

          , (a'NoteWordTODO, fg (rgbColor 255 80 (0::Int)) `withStyle` bold)
          , (a'NoteWordInProg, magenta `on` white `withStyle` bold)
          , (a'NoteWordFuture, fg (rgbColor 255 200 (0::Int)) `withStyle` bold)
          , (a'NoteWordBlocking, white `on` blue `withStyle` bold)

          , (a'NoteWordExpired, red `on` black `withStyle` bold)
          , (a'Expired, red `on` black `withStyle` bold)

          , (a'Disabled, defAttr `withStyle` dim)
          , (a'Selected, black `on` yellow)
          , (a'Error, fg red)
          , (a'Notice, fg cyan)
          ]


drawMyWork :: MyWorkState -> [Widget WName]
drawMyWork mws =
  let mainPanes =
        [
          borderWithLabel  (str $ " mywork " <> showVersion version <> " ")
          $ vBox $ catMaybes
          [
            panelDraw @SummaryPane mws
          , Just hBorder
          , Just $ hBox $ catMaybes
            [ hLimit 28
              <$> panelDraw @Projects mws
            , Just vBorder
            , Just $ vBox $ catMaybes $
              let pinfo = panelDraw @ProjInfoPane mws
              in [ pinfo
                 , const (hBorderWithLabel (str "Locations")) <$> pinfo
                 , vLimitPercent 28 <$> panelDraw @Location mws
                 , const (hBorderWithLabel (str "Notes")) <$> pinfo
                 , pinfo >> panelDraw @Note mws
                 ]
            ]
          , Just hBorder
          , panelDraw @MessagesPane mws
          , panelDraw @OperationsPane mws
          ]
        ]
      allPanes = catMaybes [ panelDraw @FileMgrPane mws
                           , panelDraw @AddProjPane mws
                           , panelDraw @LocationInputPane mws
                           , panelDraw @NoteInputPane mws
                           , panelDraw @Confirm mws
                           , panelDraw @HelpPane mws
                           ]
                 <> mainPanes
      disableLower = \case
        (m:ls) -> m : (withDefAttr a'Disabled <$> ls)
        o -> o
  in joinBorders . withBorderStyle unicode <$> disableLower allPanes
