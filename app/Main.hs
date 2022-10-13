{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Brick
import Brick.Panes
import Control.Lens
import Data.Text ( unpack )
import Graphics.Vty ( Event(EvKey), Key(KUp) )

import Defs
import Draw
import Events
import Panes.FileMgr
import Whole


main :: IO ()
main = do i <- initialState & onPane @FileMgrPane %%~ initFileMgr
          s <- defaultMain myworkApp i
          case getCurrentLocation s of
            Just (p,mbl) ->
              do let ProjectName pnm = name p
                 putStrLn $ unpack $ pnm <> ": " <> description p
                 case mbl of
                   Nothing -> return ()
                   Just l ->
                     case location l of
                       RemoteSpec r ->
                         putStrLn $ "Remote location: " <> unpack r
                       LocalSpec d -> do putStrLn "Local directory"
                                         putStrLn $ show d

            Nothing -> return ()


myworkApp :: App MyWorkState MyWorkEvent WName
myworkApp = App { appDraw = drawMyWork
                , appChooseCursor = showFirstCursor
                , appHandleEvent = handleMyWorkEvent
                , appStartEvent =
                    -- Send move-up to Projects list pane.  The cursor should
                    -- already be at the top, but this invokes the various
                    -- wrappers that will update all the panes based on the
                    -- Projects loaded by initFileMgr
                    handleMyWorkEvent (VtyEvent (EvKey KUp []))
                , appAttrMap = const myattrs
                }

