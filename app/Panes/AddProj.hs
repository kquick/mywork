{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Panes.AddProj
  (
    AddProjPane
  , initAddProj
  , isAddProjActive
  , newProject
  )
where

import           Brick
import           Brick.Focus
import           Brick.Forms
import           Brick.Panes
import           Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import           Control.Lens
import           Data.Maybe ( isJust )
import qualified Data.Sequence as Seq
import           Data.Text ( Text )
import qualified Graphics.Vty as Vty

import           Defs hiding (Location)


data AddProjPane


data NewProj = NewProj { _npName :: Text
                       , _npRole :: Role
                       , _npLangR :: Either Text Language
                       , _npLangT :: Text
                       , _npDesc :: Text
                       }

makeLenses ''NewProj


blankNewProj :: NewProj
blankNewProj = NewProj "" User (Right C) "" ""

type ProjForm = Form NewProj MyWorkEvent WName

instance Pane WName MyWorkEvent AddProjPane () where
  data (PaneState AddProjPane MyWorkEvent) = NP { nPF :: Maybe ProjForm
                                                  -- Just == pane active
                                                , nPrj :: Maybe Project
                                                  -- reset to Nothing when nPF
                                                  -- transitions Nothing -> Just
                                                }
  type (EventType AddProjPane WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  initPaneState _ = NP Nothing Nothing
  drawPane ps _gs =
    C.centerLayer
    . borderWithLabel (str "New Project")
    . vLimit 25
    . hLimitPercent 65
    . (\f -> vBox [ renderForm f
                  , vLimit 1 (fill ' ' <+> str "Ctrl-D = accept"
                              <+> fill ' ' <+> str "ESC = abort"
                              <+> fill ' ')
                  ]) <$> nPF ps
  focusable _ ps = case nPF ps of
                     Nothing -> mempty
                     Just f -> Seq.fromList $ focusRingToList $ formFocus f
  handlePaneEvent _ = \case
    VtyEvent (Vty.EvKey Vty.KEsc []) -> nPFL %%~ const (return Nothing)
    VtyEvent (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) -> \s ->
      let pf = s ^. nPFL
          np form = Project { name = form ^. npName
                            , role = form ^. npRole
                            , language = case form ^. npLangR of
                                r@(Right _) -> r
                                Left _ -> Left $ form ^. npLangT
                            , description = form ^. npDesc
                            , locations = mempty
                            }
      in return $ s & nPFL .~ Nothing & newProject .~ (np . formState <$> pf)
    ev -> nPFL . _Just %%~ \w -> nestEventM' w (handleFormEvent ev)


nPFL :: Lens' (PaneState AddProjPane MyWorkEvent) (Maybe ProjForm)
nPFL f s = (\n -> s { nPF = n }) <$> f (nPF s)

isAddProjActive :: PaneState AddProjPane MyWorkEvent -> Bool
isAddProjActive = isJust . nPF

newProject :: Lens' (PaneState AddProjPane MyWorkEvent) (Maybe Project)
newProject f s = (\n -> s { nPrj = n}) <$> f (nPrj s)


initAddProj :: PaneState AddProjPane MyWorkEvent
            -> PaneState AddProjPane MyWorkEvent
initAddProj ps =
  case nPF ps of
    Just _ -> ps
    Nothing ->
      let label s = padBottom (Pad 1) . label' s
          label' s w = (vLimit 1 $ hLimit 15 $ fill ' ' <+> str s <+> str ": ") <+> w
          npForm =
            newForm
            [ label "Project name" @@=
              editTextField npName (WName "+Prj:Name") (Just 1)
            , label "Role" @@=
              radioField npRole
              [ (Author, (WName "+Prj:Role:Author"), "Author")
              , (Maintainer, (WName "+Prj:Role:Maintainer"), "Maintainer")
              , (Contributor, (WName "+Prj:Role:Contributor"), "Contributor")
              , (User, (WName "+Prj:Role:User"), "User")
              ]
            , label' "Language" @@=
              radioField npLangR
              [ (Right C, (WName "+Prj:Lang:C"), "C")
              , (Right CPlusPlus, (WName "+Prj:Lang:CPP"), "C++")
              , (Right Haskell, (WName "+Prj:Lang:Haskell"), "Haskell")
              , (Left "", (WName "+Prj:Lang:Other"), "Custom")
              ]
            , (\w -> padBottom (Pad 1) $ vLimit 1 $ padLeft (Pad 19)
                     $ str "Custom value: " <+> w) @@=
              editTextField npLangT (WName "+Prj:Lang:Text") (Just 1)
            , label "Description" @@=
              editTextField npDesc (WName "+Prj:Desc") Nothing
            ]
            blankNewProj
      in NP { nPF = Just npForm, nPrj = Nothing }
