module Text.Regex.Do.Pcre.Result where

import qualified Data.Array as A(elems)
import Text.Regex.Base.RegexLike as R
import Text.Regex.Do.TypeDo

-- | match offset, length
poslen::Functor f =>
    f MatchArray -> f [PosLen]
poslen = (A.elems <$>)


-- | all groups
allMatches::(Functor f, R.Extract a) =>
    Body a -> f MatchArray -> f [a]
allMatches hay0 results0 = groupMatch hay0 <$> results0


-- | matches for one group
groupMatch::R.Extract a =>
    Body a -> MatchArray -> [a]
groupMatch (Body hay1) arr1 = [R.extract tuple1 hay1 |  tuple1 <- A.elems arr1]
