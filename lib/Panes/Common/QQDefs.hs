{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Panes.Common.QQDefs where

import Data.String
import Language.Haskell.TH ( ExpQ )
import Language.Haskell.TH.Quote


sq :: QuasiQuoter
sq = QuasiQuoter extractor
     (error "no q patterns")
     (error "no q types")
     (error "no q dec")

extractor :: String -> ExpQ
extractor s =
  case inSeps $ filter (/= '\r') s of
    Post a -> [|fromString a|]
    Pre _ -> error $ "No starting line found"
    MatchLine -> error $ "Only starting line found"
    Pass _ -> error $ "No ending line found"


data QState = Pre String | MatchLine | Pass String | Post String

inSeps :: String -> QState
inSeps =
  let sep = "\n----"
      sepl = length sep
      nxtC :: QState -> Char -> QState
      nxtC (Pre p) c = let p' = c : p
                           l = length p'
                       in if reverse p' == take l sep
                          then if l == sepl then MatchLine else Pre p'
                          else Pre $ take (sepl-1) p'
      nxtC MatchLine c = if '\n' == c then Pass "" else MatchLine
      nxtC (Pass s) c = let s' = c : s
                            sl = reverse $ take sepl s'
                        in if sl == sep
                           then Post (reverse $ drop sepl s')
                           else Pass s'
      nxtC (Post s) _ = Post s
    in foldl nxtC (Pre "")
