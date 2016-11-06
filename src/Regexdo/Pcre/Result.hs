module Regexdo.Pcre.Result
    (poslen,
    value,
    oneMatchArray,
    R.extract   -- | 'extract' is reexport from "Text.Regex.Base.RegexLike"
    ) where

import qualified Data.Array as A(elems)
import Text.Regex.Base.RegexLike as R
import Regexdo.TypeDo


poslen::Functor f =>
    f MatchArray -> f [PosLen]
poslen = (A.elems <$>)


-- | all matches
value::(Functor f, R.Extract a) =>
    Haystack a -> f MatchArray -> f [a]
value hay0 results0 = oneMatchArray hay0 <$> results0


-- | extracts all values in a MatchArray (matched group)
oneMatchArray::R.Extract a =>
    Haystack a -> MatchArray -> [a]
oneMatchArray (Haystack hay1) arr1 = [R.extract tuple1 hay1 |  tuple1 <- A.elems arr1]
