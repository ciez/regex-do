{-# LANGUAGE DeriveFunctor #-}
module Regexdo.TypeDo where

import Text.Regex.Base.RegexLike

-- pcre
type GroupReplacer acc = (MatchArray -> acc -> acc) -- MatchArray -> acc -> acc


-- stringsearch, Pcre.Replace
data Needle n = Needle n deriving (Functor)          -- Bs, String, RegexPcre
data Haystack h = Haystack h deriving (Functor)                -- Bs, String
data Replacement r = Replacement r deriving (Functor)     --    Bs, String

type PosLen = (MatchOffset, MatchLength)