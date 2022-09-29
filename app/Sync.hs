{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Sync
  -- (
  -- )
where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( foldM )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.HashMap.Lazy as HM
import           Data.Ini
import qualified Data.List as DL
import           Data.Maybe ( catMaybes )
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Calendar ( Day )
import           Data.Time.Clock ( getCurrentTime, utctDay )
import           Path ( (</>), relfile, toFilePath, parseAbsDir )
import           Path.IO ( doesDirExist, doesFileExist, getModificationTime )

import           Defs


data LocationStatus = LocationStatus { locExists :: Maybe Bool
                                     , otherLocs :: [ (LType, LocationSpec) ]
                                     , lastUpd :: Maybe Day
                                     }

data LType = GitRepo GitRemote | GitFork LType
  deriving Eq

newtype GitRemote = GitRemote Text
  deriving Eq


syncLocation :: MonadIO m => Location -> m LocationStatus
syncLocation l = case location l of
  LocalSpec lcl ->
    do e <- liftIO $ doesDirExist lcl
       u <- if e
            then Just . utctDay <$> liftIO (getModificationTime lcl)
            else return Nothing
       o <- let gitCfgFile = lcl </> [relfile|.git/config|]
                gcRemote sname cfg locs =
                  case T.words sname of
                    ["remote", rmt] ->
                      let t = GitRepo $ GitRemote
                              -- drop surrounding double-quotes
                              $ T.drop 1 $ T.take (T.length rmt - 1) rmt
                      in locs <> catMaybes
                         [ (t,) . toLocSpec <$> HM.lookup "url" cfg
                         , (GitFork t,) . toLocSpec <$> HM.lookup "pushurl" cfg
                         ]
                    _ -> locs
                toLocSpec t = maybe (RemoteSpec t) LocalSpec
                              $ parseAbsDir $ T.unpack t
                gcProc = HM.foldrWithKey gcRemote mempty . unIni
            in do ge <- liftIO (doesFileExist gitCfgFile)
                  if ge
                    then do gt <- liftIO $ TIO.readFile (toFilePath gitCfgFile)
                            case parseIni gt of
                              Left _e -> return mempty -- no error reporting
                              Right gc -> return $ gcProc gc
                    else return mempty
       return $ LocationStatus { locExists = Just e
                               , otherLocs = o
                               , lastUpd = u
                               }
  RemoteSpec _ -> return LocationStatus { locExists = Nothing
                                        , otherLocs = mempty
                                        , lastUpd = Nothing
                                        }


applyLocSync :: Day -> LocationStatus -> Location -> Location
applyLocSync now locsts loc =
  let rmtnoteTxt :: (LType, LocationSpec) -> Text
      rmtnoteTxt = \case
        (GitRepo (GitRemote n), r) ->
          "Cloned from git repo " <> tshow n <> " @ " <> tshow r
        (GitFork (GitRepo (GitRemote n)), r) ->
          "Pushing to git repo " <> tshow n <> " fork @ " <>  tshow r
        (_, r) -> "Related to " <> tshow r
      addRmtNoteText ol cl =
        -- n.b. instead of using updateNote, which prefers the new note, this
        -- only adds a note if there isn't already one, preferring the existing
        -- one in case it has been updated (aside from the noteTitle).
        let rnt = rmtnoteTxt ol
            rn = Note { note = rnt, notedOn = now, noteSource = MyWorkGenerated }
        in case DL.find ((noteTitle' rnt ==) . noteTitle) (notes cl) of
             Nothing -> cl { notes = rn : notes cl }
             Just _ -> cl
      loc' = foldr addRmtNoteText loc $ otherLocs locsts
  in loc' { locValid = maybe True id $ locExists locsts
          , locatedOn = lastUpd locsts <|> locatedOn loc
          }

applyProjLocSync :: MonadIO m
                 => Maybe LocationSpec -> Project -> Location -> m Project
applyProjLocSync mbOldL p l =
  do locsts <- syncLocation l
     now <- utctDay <$> liftIO getCurrentTime
     let p' = updateLocation mbOldL (applyLocSync now locsts l) p
     let rmtspec rmtName =
           DL.lookup (GitRepo (GitRemote rmtName)) $ otherLocs locsts
     let mkLoc (lt,ls) =
           let nts = case lt of
                       GitRepo (GitRemote _) -> mempty
                       GitFork (GitRepo (GitRemote n)) ->
                         [ Note { note = "Fork of git repo @ " <>
                                         case rmtspec n of
                                           Just rls -> tshow rls
                                           Nothing -> "??"
                                , notedOn = now
                                , noteSource = MyWorkGenerated
                                }
                         ]
                       _ -> [ Note { note = "Related to " <> tshow ls
                                   , notedOn = now
                                   , noteSource = MyWorkGenerated
                                   }
                            ]
           in Location { location = ls
                       , locatedOn = Nothing
                       , locValid = True
                       , notes = nts
                       }
     foldM (applyProjLocSync Nothing) p' (mkLoc <$> otherLocs locsts)


syncProject :: MonadIO m => Project -> m Project
syncProject p = foldM (applyProjLocSync Nothing) p $ locations p
