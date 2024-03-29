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
import qualified Brick.Widgets.Core as BC
import qualified Brick.Widgets.Table as BT
import           Control.Lens hiding ( under )
import           Data.Either ( isRight )
import qualified Data.List as DL
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


data NewProj = NewProj { _npName :: ProjectName
                       , _npRole :: Role
                       , _npGroupG :: Maybe Group
                       , _npGroupT :: Text
                       , _npLangR :: Either Text Language
                       , _npLangT :: Text
                       , _npDesc :: Text
                       , _npLoc :: LocationSpec
                       , _npLocDate :: Maybe Day
                       }

makeLenses ''NewProj


blankNewProj :: NewProj
blankNewProj = NewProj (ProjectName "") User (Just Personal) "" (Right C)
               "" "" (RemoteSpec "") Nothing

type ProjForm = Form NewProj MyWorkEvent WName

instance Pane WName MyWorkEvent AddProjPane where
  data (PaneState AddProjPane MyWorkEvent) =
    NP { nPF :: Maybe ProjForm
         -- Just == pane active
       , nPrj :: Maybe Project
         -- ^ the new project information to be added to the mywork database.
         -- Automatically reset to Nothing when this modal is enabled (i.e. nPF
         -- transitions Nothing -> Just)
       , nOrig :: Maybe Project
         -- ^ if editing a project, this is the original project.
       , nErr :: Maybe Text
       }
  type (EventType AddProjPane WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  initPaneState _ = NP Nothing Nothing Nothing Nothing
  drawPane ps _gs =
    C.centerLayer
    . modalB ((maybe "New" (const "Edit") $ nOrig ps) <> " Project")
    . vLimitPercent 80
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

  -- | When adding or editing a project, ESC cancels and C-d (done) completes the
  -- edit.  If this is an add, ESC simply dismisses this modal and Ctrl-D
  handlePaneEvent _ = \case
    VtyEvent (Vty.EvKey Vty.KEsc []) -> nPFL %%~ const (return Nothing)
    VtyEvent (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) -> \s ->
      let pf = s ^. nPFL
          np form = Project { projName = form ^. npName
                            , group = case form ^. npGroupG of
                                Just r -> r
                                Nothing -> OtherGroup $ form ^. npGroupT
                            , role = form ^. npRole
                            , language = case form ^. npLangR of
                                r@(Right _) -> r
                                Left _ -> Left $ form ^. npLangT
                            , description = form ^. npDesc
                            , locations =
                                case form ^. npLoc of
                                  RemoteSpec rs | T.null rs -> mempty
                                  _ -> [ Location
                                         { location = form ^. npLoc
                                         , locatedOn = form ^. npLocDate
                                         , notes = mempty
                                         , locCore = LocRT
                                                     { locValid = True -- assumed
                                                     }
                                         }
                                       ]
                            , projCore = ProjRT
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


-- | Lens to access the ProjForm (modal Form entry widget) from the pane.  If
-- Nothing, this pane is not active or drawn.
nPFL :: Lens' (PaneState AddProjPane MyWorkEvent) (Maybe ProjForm)
nPFL f s = (\n -> s { nPF = n }) <$> f (nPF s)

isAddProjActive :: PaneState AddProjPane MyWorkEvent -> Bool
isAddProjActive = isJust . nPF

newProject :: Lens' (PaneState AddProjPane MyWorkEvent) (Maybe Project)
newProject f s = (\n -> s { nPrj = n}) <$> f (nPrj s)


-- | Returns the original project name (if any) and the new Project
-- specification.
projectInputResults :: PaneState AddProjPane MyWorkEvent
                     -> (Maybe ProjectName, Maybe Project)
projectInputResults ps = (view projNameL <$> nOrig ps, nPrj ps)


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
          numCols lastSolo nc =
            let go wdgs =
                  if null wdgs then []
                  else fmap padded (DL.take nc (wdgs <> DL.repeat emptyWidget))
                       : go (DL.drop nc wdgs)
                padded = padRight (BC.Pad 2)
                renderT = BT.renderTable
                          . BT.surroundingBorder False
                          . BT.rowBorders False
                          . BT.columnBorders False
            in if lastSolo
               then \wdgs ->
                      if null wdgs then emptyWidget
                      else (renderT $ BT.table $ go $ DL.init wdgs)
                           <=> DL.last wdgs
               else renderT . BT.table . go
          projFields =
            [ label "Project name" @@=
              let validate = \case
                    [] -> Nothing
                    [""] -> Nothing
                    (nmt:_) ->
                      let nm = ProjectName nmt
                      in if nm `elem` (view projNameL <$> projects prjs)
                            && (maybe True ((nm /=) . view projNameL) mbProj)
                         then Nothing  -- invalid
                         else Just nm
              in editField npName (WName "New Project Name") (Just 1)
                 (\(ProjectName nm) -> nm) validate (txt . headText) id
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
              setFieldConcat (numCols False 2)
              . radioField npRole
              [ (Author, (WName "+Prj:Role:Author"), "Author")
              , (Maintainer, (WName "+Prj:Role:Maintainer"), "Maintainer")
              , (Contributor, (WName "+Prj:Role:Contributor"), "Contributor")
              , (User, (WName "+Prj:Role:User"), "User")
              ]
            , label' "Language" @@=
              setFieldConcat (numCols True 4)
              . radioField npLangR
              [ (Right C, (WName "+Prj:Lang:C"), "C")
              , (Right CPlusPlus, (WName "+Prj:Lang:CPP"), "C++")
              , (Right Haskell, (WName "+Prj:Lang:Haskell"), "Haskell")
              , (Right JavaScript, (WName "+Prj:Lang:JS"), "JavaScript")
              , (Right Prolog, (WName "+Prj:Lang:Prolog"), "Prolog")
              , (Right Python, (WName "+Prj:Lang:Python"), "Python")
              , (Right Rust, (WName "+Prj:Lang:Rust"), "Rust")
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
               Just p -> NewProj { _npName = p ^. projNameL
                                 , _npRole = p ^. roleL
                                 , _npGroupG = case p ^. groupL of
                                                 Personal -> Just Personal
                                                 Work -> Just Work
                                                 OtherGroup _ -> Nothing
                                 , _npGroupT = case p ^. groupL of
                                                 OtherGroup t -> t
                                                 _ -> ""
                                 , _npLangR = p ^. languageL
                                 , _npLangT = case p ^. languageL of
                                                Right _ -> ""
                                                Left t -> t
                                 , _npDesc = p ^. descriptionL
                                 , _npLoc = RemoteSpec ""
                                 , _npLocDate = Nothing
                                 }
            )
      in NP { nPF = Just npForm
            , nPrj = Nothing
            , nOrig = mbProj
            , nErr = Nothing
            }
