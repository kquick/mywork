{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Panes.Summary
  (
    SummaryPane
  )
where

import           Brick
import           Brick.Panes
import qualified Data.List as List
import           Data.Maybe ( catMaybes )

import           Defs


data SummaryPane

instance Pane WName MyWorkEvent SummaryPane () where
  data (PaneState SummaryPane MyWorkEvent) = Unused
  type (DrawConstraints SummaryPane s WName) = ( HasProjects s )
  initPaneState _ = Unused
  drawPane _ s = Just $ drawSummary (snd $ getProjects s)


drawSummary :: Projects -> Widget WName
drawSummary prjcts =
    let prjs = projects prjcts
        prjcnt = str $ "# Projects=" <> show (length prjs) <> subcounts
        subcounts = (" (" <>)
                    $ (<> ")")
                    $ List.intercalate ", "
                    [ show r <> "=" <> show (length fp)
                    | r <- [minBound .. maxBound]
                    , let fp = filter (isRole r) prjs
                    , not (null fp)
                    ]
        isRole r p = r == role p
        dateRange = str (show (minimum projDates)
                         <> ".."
                         <> show (maximum projDates)
                        )
        locDates prj = catMaybes (locatedOn <$> locations prj)
        projDates = concatMap locDates prjs
    in vLimit 1
       $ if null prjs
         then str "No projects defined"
         else prjcnt <+> fill ' ' <+> dateRange
