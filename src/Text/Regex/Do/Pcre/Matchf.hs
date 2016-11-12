-- | __internal__ module, exposed only to show the instances
module Text.Regex.Do.Pcre.Matchf where

import Text.Regex.Do.Type.Do
import Text.Regex.Do.Type.Reexport as R
import Text.Regex.Do.Result as R
import Text.Regex.Base.RegexLike as R
import Text.Regex.Do.Type.MatchHint

type R_ b = R.RegexLike R.Regex b


class Matchf hint b where
    type H hint
    type P hint
    marray_::R_ b => hint (Pattern R.Regex) -> Body b -> H hint
    poslen_::R_ b => hint (Pattern R.Regex) -> Body b -> P hint

instance Matchf Once b where
    type H Once = Maybe MatchArray
    type P Once = Maybe [PosLen]
    marray_ (Once (Pattern p0)) (Body b0) = R.matchOnce p0 b0
    poslen_ r0 b0 = R.poslen $ marray_ r0 b0

instance Matchf All b where
    type H All = [MatchArray]
    type P All = [[PosLen]]
    marray_ (All (Pattern p0)) (Body b0) = R.matchAll p0 b0
    poslen_ r0 b0 = R.poslen $ marray_ r0 b0


once::(R_ b, R.Extract b) =>
    Pattern R.Regex -> Body b -> [b]      -- ^ matched content
once p0 b0 = maybe [] id $ R.allMatches b0 $ marray_ (Once p0) b0


all::(R_ b, R.Extract b) =>
    Pattern R.Regex -> Body b -> [[b]]       -- ^ matched content
all p0 b0 = R.allMatches b0 $ marray_ (All p0) b0
