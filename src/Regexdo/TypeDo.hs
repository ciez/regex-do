module Regexdo.TypeDo where

import Text.Regex.Base.RegexLike

-- pcre
type GroupReplacer a = (MatchArray -> ReplaceAcc a -> ReplaceAcc a) -- MatchArray -> acc -> acc


data ReplaceAcc a = ReplaceAcc {
    acc_haystack::a,   -- ^ wip Haystack
    position_adj::Int
    }


-- stringsearch, Pcre.Replace
data Needle n = Needle n deriving (Functor)          -- Bs, String, RegexPcre
data Haystack h = Haystack h deriving (Functor)                -- Bs, String
data Replacement r = Replacement r deriving (Functor)     --    Bs, String

type PosLen = (MatchOffset, MatchLength)


data ReplaceCase = Once     -- ^ may be omitted
                | All       -- ^ if both Once and All are passed, All prevails
                | Utf8
                | Multiline deriving Eq


