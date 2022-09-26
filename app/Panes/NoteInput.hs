{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Panes.NoteInput
  (
    NoteInputPane
  , initNoteInput
  , isNoteInputActive
  , noteInputResults
  )
where

import           Brick hiding ( Location )
import           Brick.Focus
import           Brick.Forms
import           Brick.Panes
import           Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import           Control.Lens
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Maybe ( isJust )
import qualified Data.Sequence as Seq
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day )
import           Data.Time.Clock
import qualified Graphics.Vty as Vty

import           Defs
import           Panes.Common.Inputs


data NoteInputPane


data NewNote = NewNote { _nnText :: Text
                       , _nnDay :: Day
                       }

makeLenses ''NewNote


blankNewNote :: NewNote
blankNewNote = NewNote "" (toEnum 0)

type NoteForm = Form NewNote MyWorkEvent WName

instance Pane WName MyWorkEvent NoteInputPane () where
  data (PaneState NoteInputPane MyWorkEvent) = NN { nNF :: Maybe NoteForm
                                                    -- Just == pane active
                                                  , nNote :: Maybe Note
                                                    -- reset to Nothing when
                                                    -- nNF transitions Nothing
                                                    -- to Just
                                                  -- KWQ , nProj :: Text
                                                  , nOrig :: Maybe Note
                                                  , nErr :: Maybe Text
                                                }
  type (EventType NoteInputPane WName MyWorkEvent) = BrickEvent WName MyWorkEvent
  initPaneState _ = NN Nothing Nothing Nothing Nothing
  drawPane ps _gs =
    C.centerLayer
    . borderWithLabel (str $ (maybe "New" (const "Edit") $ nOrig ps)
                       -- <> " " <> show (nProj ps)
                       <> " Note")
    . vLimit 25
    . hLimitPercent 65
    . (\f -> vBox [ renderForm f
                  , padBottom (Pad 1) $ withAttr a'Error
                    $ maybe emptyWidget txt (nErr ps)
                  , emptyWidget
                  , vLimit 1 (fill ' ' <+> str "Ctrl-D = accept"
                              <+> fill ' ' <+> str "ESC = abort"
                              <+> fill ' ')
                  ]) <$> nNF ps
  focusable _ ps = case nNF ps of
                     Nothing -> mempty
                     Just f -> Seq.fromList $ focusRingToList $ formFocus f
  handlePaneEvent _ = \case
    VtyEvent (Vty.EvKey Vty.KEsc []) -> nNFL %%~ const (return Nothing)
    VtyEvent (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) -> \s ->
      let pf = s ^. nNFL
          np form = Note { note = form ^. nnText
                         , notedOn = form ^. nnDay
                         }
      in if maybe False allFieldsValid pf
         then
           return $ s & nNFL .~ Nothing & newNote .~ (np . formState <$> pf)
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
                & (nNFL . _Just %%~ \w -> nestEventM' w (handleFormEvent ev))


nNFL :: Lens' (PaneState NoteInputPane MyWorkEvent) (Maybe NoteForm)
nNFL f s = (\n -> s { nNF = n }) <$> f (nNF s)

isNoteInputActive :: PaneState NoteInputPane MyWorkEvent -> Bool
isNoteInputActive = isJust . nNF


newNote :: Lens' (PaneState NoteInputPane MyWorkEvent) (Maybe Note)
newNote f s = (\n -> s { nNote = n }) <$> f (nNote s)


-- | Returns the original note name (if any) and the new Note
-- specification.
noteInputResults :: PaneState NoteInputPane MyWorkEvent
                 -> (Maybe Text, Maybe Note)
noteInputResults ps = (note <$> nOrig ps, nNote ps)


-- KWQ: remove!?
validateForm :: EventM WName es (PaneState NoteInputPane MyWorkEvent)
             -> EventM WName es (PaneState NoteInputPane MyWorkEvent)
validateForm inner = do
  s <- inner
  case s ^. nNFL of
    Nothing -> return s
    Just _f -> return s


initNoteInput :: MonadIO m
              => [Note]
              -> Maybe Note
              -> PaneState NoteInputPane MyWorkEvent
              -> m (PaneState NoteInputPane MyWorkEvent)
initNoteInput curNotes mbNote ps = do
  now <- utctDay <$> liftIO getCurrentTime
  case nNF ps of
    Just _ -> return ps
    Nothing ->
      let label s = padBottom (Pad 1) . label' s
          label' s w = (vLimit 1 $ hLimit labelWidth
                        $ fill ' ' <+> str s <+> str ": ") <+> w
          labelWidth = 15
          nlForm =
            newForm
            [
              label "Note" @@=
              let validate = \case
                    [] -> Nothing
                    [""] -> Nothing
                    o@(l:_) -> if (l `elem` (noteTitle <$> curNotes)
                                   && (maybe True ((l /=) . noteTitle) mbNote))
                               then Nothing  -- invalid
                               else Just $ T.intercalate "\n" o
              in editField nnText (WName "New Note")
                 Nothing
                 -- (Just 20)
                 id validate (txt . T.intercalate "\n") id
            , label "Date" @@= let validate = \case
                                     ("":_) -> Nothing
                                     (l:_) -> textToDay l
                                     _ -> Nothing
                                   dayInit = T.pack . show
                                   dayRender = txt . headText
                               in editField nnDay (WName "Note Date (Y-M-D)")
                                  (Just 1) dayInit validate dayRender id
            ]
            (case mbNote of
               Nothing -> blankNewNote { _nnDay = now }
               Just l -> NewNote { _nnText = note l
                                 , _nnDay = notedOn l
                                 }
            )
      in return $ NN { nNF = Just nlForm
                     -- , nProj = projName
                     , nNote = Nothing
                     , nOrig = mbNote
                     , nErr = Nothing
                     }
