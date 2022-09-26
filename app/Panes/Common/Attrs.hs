module Panes.Common.Attrs where

import Data.Text ( Text )
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Defs


-- | Adds a border with a title to the current widget.  First argument is True if
-- the current widget has focus.
titledB :: Bool -> Text -> Widget WName -> Widget WName
titledB fcsd text =
  let ttlAttr = if fcsd then withAttr (attrName "Selected") else id
  in borderWithLabel (ttlAttr $ txt text)


modalB :: Text -> Widget WName -> Widget WName
modalB t = withBorderStyle unicodeBold . borderWithLabel (txt t)
