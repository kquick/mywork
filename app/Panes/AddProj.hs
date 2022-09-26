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
  , projectInputResults
  )
where

import           Brick hiding ( Location )
import           Brick.Focus
import           Brick.Forms
import           Brick.Panes
import qualified Brick.Widgets.Center as C
import           Control.Lens hiding ( under )
import           Data.Either ( isRight )
import           Data.Maybe ( isJust )
import qualified Data.Sequence as Seq
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day )
import qualified Graphics.Vty as Vty

import           Defs
import           Panes.Common.Attrs
import           Panes.Common.Inputs
import           Sync


data AddProjPane


data NewProj = NewProj { _npName :: Text
                       , _npRole :: Role
                       , _npGroupG :: Maybe Group
                       , _npGroupT :: Text
                       , _npLangR :: Either Text Language
                       , _npLangT :: Text
                       , _npDesc :: Text
                       , _npLoc :: Text
                       , _npLocDate :: Maybe Day
                       }

makeLenses ''NewProj


blankNewProj :: NewProj
blankNewProj = NewProj "" User (Just Personal) "" (Right C) "" "" "" Nothing

type ProjForm = Form NewProj MyWorkEvent WName

instance Pane WName MyWorkEvent AddProjPane () where
  data (PaneState AddProjPane MyWorkEvent) = NP { nPF :: Maybe ProjForm
                                                  -- Just == pane active
                                                , nPrj :: Maybe Project
                                                  -- reset to Nothing when nPF
                                                  -- transitions Nothing -> Just
                                                , nOrig :: Maybe Project
                                                , nErr :: Maybe Text
                                                }
  type (EventType AddProjPane WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  initPaneState _ = NP Nothing Nothing Nothing Nothing
  drawPane ps _gs =
    C.centerLayer
    . modalB ((maybe "New" (const "Edit") $ nOrig ps) <> " Project")
    . vLimit 25
    . hLimitPercent 65
    . (\f -> vBox [ renderForm f
                  , padBottom (Pad 1) $ withAttr a'Error
                    $ maybe (txt " ") txt (nErr ps)
                  , emptyWidget
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
                            , group = case form ^. npGroupG of
                                Just r -> r
                                Nothing -> OtherGroup $ form ^. npGroupT
                            , role = form ^. npRole
                            , language = case form ^. npLangR of
                                r@(Right _) -> r
                                Left _ -> Left $ form ^. npLangT
                            , description = form ^. npDesc
                            , locations = if T.null (form ^. npLoc)
                                          then mempty
                                          else [ Location
                                                 { location = form ^. npLoc
                                                 , locatedOn = form ^. npLocDate
                                                 , locValid = True -- assumed
                                                 , notes = mempty
                                                 }
                                               ]
                            }
      in if maybe False allFieldsValid pf
         then do let p0 = np . formState <$> pf
                 p <- case p0 of
                        Nothing -> return Nothing
                        Just jp -> Just <$> syncProject jp
                 return $ s & nPFL .~ Nothing & newProject .~ p
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
                & (nPFL . _Just %%~ \w -> nestEventM' w (handleFormEvent ev))


nPFL :: Lens' (PaneState AddProjPane MyWorkEvent) (Maybe ProjForm)
nPFL f s = (\n -> s { nPF = n }) <$> f (nPF s)

isAddProjActive :: PaneState AddProjPane MyWorkEvent -> Bool
isAddProjActive = isJust . nPF

newProject :: Lens' (PaneState AddProjPane MyWorkEvent) (Maybe Project)
newProject f s = (\n -> s { nPrj = n}) <$> f (nPrj s)


-- | Returns the original project name (if any) and the new Project
-- specification.
projectInputResults :: PaneState AddProjPane MyWorkEvent
                     -> (Maybe Text, Maybe Project)
projectInputResults ps = (name <$> nOrig ps, nPrj ps)


validateForm :: EventM WName es (PaneState AddProjPane MyWorkEvent)
             -> EventM WName es (PaneState AddProjPane MyWorkEvent)
validateForm inner = do
  s <- inner
  case s ^. nPFL of
    Nothing -> return s
    Just pf -> do
      let isOK1 = or [ formState pf ^. npGroupG /= Nothing
                     , formState pf ^. npGroupT /= ""
                     ]
      let tgtfld1 = WName "Other Group Text"
      let isOK2 = or [ isRight (formState pf ^. npLangR)
                     , formState pf ^. npLangT /= ""
                     ]
      let tgtfld2 = WName "Other Language Name"
      (ltgt, lvalid) <- validateLocationInput True $ formState pf ^. npLoc
      return $ s
        & nPFL %~ fmap (setFieldValid isOK1 tgtfld1)
        & nPFL %~ fmap (setFieldValid isOK2 tgtfld2)
        & nPFL %~ fmap (setFieldValid lvalid ltgt)


initAddProj :: Projects
            -> Maybe Project
            -> PaneState AddProjPane MyWorkEvent
            -> PaneState AddProjPane MyWorkEvent
initAddProj prjs mbProj ps =
  case nPF ps of
    Just _ -> ps
    Nothing ->
      let label s = padBottom (Pad 1) . label' s
          label' s w = (vLimit 1 $ hLimit labelWidth
                        $ fill ' ' <+> str s <+> str ": ") <+> w
          under s w = padBottom (Pad 1)
                      $ vLimit 1
                      $ padLeft (Pad (labelWidth + 4))
                      $ str s <+> w
          labelWidth = 18
          projFields =
            [ label "Project name" @@=
              let validate = \case
                    [] -> Nothing
                    [""] -> Nothing
                    (nm:_) -> if nm `elem` (name <$> projects prjs) &&
                                 (maybe True ((nm /=) . name) mbProj)
                              then Nothing  -- invalid
                              else Just nm
              in editField npName (WName "New Project Name") (Just 1)
                 id validate (txt . headText) id
            , label' "Group" @@=
              radioField npGroupG
              [ (Just Personal, (WName "+Prj:Grp:Personal"), "Personal")
              , (Just Work, (WName "+Prj:Grp:Work"), "Work")
              , (Nothing, (WName "Other Group Text"), "Other")
              ]
            , under "...: " @@=
              editTextField npGroupT (WName "+Proj:Grp:Text") (Just 1)
            , label "Role" @@=
              -- setFieldConcat (hBox . DL.intersperse (str "  ")) .
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
              , (Left "", (WName "Other Language Name"), "Custom")
              ]
            , under "...: " @@=
              editTextField npLangT (WName "+Prj:Lang:Text") (Just 1)
            , label "Description" @@=
              editTextField npDesc (WName "+Prj:Desc") Nothing
            ]
          locFields =
            case mbProj of
              Nothing ->
                -- Only query for the initial location if this is a new project;
                -- do not query for an existing project.
                [
                  label "Initial location" @@=
                  locationInput mempty Nothing True npLoc
                , label "Location date" @@= mbDateInput npLocDate
                ]
              _ -> []
          npForm =
            newForm (projFields <> locFields)
            (case mbProj of
               Nothing -> blankNewProj
               Just p -> NewProj { _npName = name p
                                 , _npRole = role p
                                 , _npGroupG = case group p of
                                                 Personal -> Just Personal
                                                 Work -> Just Work
                                                 OtherGroup _ -> Nothing
                                 , _npGroupT = case group p of
                                                 OtherGroup t -> t
                                                 _ -> ""
                                 , _npLangR = language p
                                 , _npLangT = case language p of
                                                Right _ -> ""
                                                Left t -> t
                                 , _npDesc = description p
                                 , _npLoc = ""
                                 , _npLocDate = Nothing
                                 }
            )
      in NP { nPF = Just npForm
            , nPrj = Nothing
            , nOrig = mbProj
            , nErr = Nothing
            }
