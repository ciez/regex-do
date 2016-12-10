module Text.Regex.Do.Match.Result where

import qualified Data.Array as A(elems)
import Text.Regex.Base.RegexLike as R
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Type.Internal


-- | match offset, length
poslen::Functor f =>
    f MatchArray -> f [PosLen]
poslen = (A.elems <$>)


-- | all groups
allMatches::(Functor f, R.Extract b) =>
    Body b -> f MatchArray -> f [b]
allMatches hay0 results0 = groupMatch hay0 <$> results0


-- | matches for one group
groupMatch::R.Extract b =>
    Body b -> MatchArray -> [b]
groupMatch (Body b0) a0 = [R.extract tuple1 b0 |  tuple1 <- A.elems a0]
