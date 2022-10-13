{-# LANGUAGE TemplateHaskell #-}

module Defs.Lenses
  (
    makeLensL
  )
where

import           Control.Lens
import qualified Language.Haskell.TH as TH

-- | This is a helper function use to create Lenses for structure
-- fields using Template Haskell.  It is similar to the 'makeLenses'
-- function provided by the 'lens' library but instead of converting
-- fields with a name of "_{fieldname}" to a lens of "fieldname", the
-- 'makeLensL' converts a field name of "{fieldname}" to a lens named
-- "fieldnameL" (appends an 'L' instead of removing a '_' prefix).

makeLensL :: TH.Name -> TH.DecsQ
makeLensL = makeLensesWith
            (lensRules
             & lensField
              .~ (\_ _ n -> [TopName $ TH.mkName $ TH.nameBase n <> "L"])
            )
