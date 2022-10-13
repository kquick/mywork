{-# LANGUAGE TypeFamilies #-}

module Defs.Static
  -- (
  -- )
  where

import Control.Lens

import Defs


type instance ProjectCore () = Maybe ()
type instance LocationCore () = Maybe ()
type instance NoteCore () = Maybe ()

hydrate :: Project_ () -> Project_ Live
hydrate dp = Project { projName = dp ^. projNameL
                     , group = dp ^. groupL
                     , role = dp ^. roleL
                     , description = dp ^. descriptionL
                     , language = dp ^. languageL
                     , locations = hydrateLoc <$> (dp ^. locationsL)
                     , projCore = ProjRT
                     }

hydrateLoc :: Location_ () -> Location_ Live
hydrateLoc dl = Location { location = dl ^. locationL
                         , locatedOn = dl ^. locatedOnL
                         , notes = hydrateNote <$> (dl ^. notesL)
                         , locCore = LocRT
                                     { locValid = True  -- default
                                     }
                         }

hydrateNote :: Note_ () -> Note_ Live
hydrateNote dn = Note { notedOn = dn ^. notedOnL
                      , note = dn ^. noteL
                      , noteCore = NoteRT
                                   { noteSource = MyWorkDB
                                   }

                      }


dehydrate :: Project_ Live -> Project_ ()
dehydrate hp = Project { projName = hp ^. projNameL
                       , group = hp ^. groupL
                       , role = hp ^. roleL
                       , description = hp ^. descriptionL
                       , language = hp ^. languageL
                       , locations = dehydrateLoc <$> (hp ^. locationsL)
                       , projCore = Nothing
                       }

dehydrateLoc :: Location_ Live -> Location_ ()
dehydrateLoc hl = Location { location = hl ^. locationL
                           , locatedOn = hl ^. locatedOnL
                             -- only emit static notes, not dynamically generated
                             -- notes.
                           , notes =
                               let isStatic = (MyWorkDB ==) . view noteSourceL
                               in dehydrateNote
                                  <$> (filter isStatic $ hl ^. notesL)
                           , locCore = Nothing
                           }

dehydrateNote :: Note_ Live -> Note_ ()
dehydrateNote hn = Note { notedOn = hn ^. notedOnL
                        , note = hn ^. noteL
                        , noteCore = Nothing
                        }
