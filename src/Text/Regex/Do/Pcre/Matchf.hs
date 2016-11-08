-- | __internal__ module, exposed only to show the instances
module Text.Regex.Do.Pcre.Matchf where

import Text.Regex.Do.TypeDo
import Text.Regex.Do.TypeRegex
import qualified Text.Regex.Do.Pcre.Result as R
import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)


class (Rx_ n h, Functor f) => Matchf f n h where
   marray_::Pattern n -> Body h -> f MatchArray
   poslen_::Pattern n -> Body h -> f [PosLen]
   poslen_ r0 b0 = R.poslen $ marray_ r0 b0

instance Rx_ n h => Matchf Maybe n h where
   marray_ r0 (Body b0) = R.matchOnce (r_ r0) b0

instance Rx_ n h => Matchf [] n h where
   marray_ r0 (Body b0) = R.matchAll (r_ r0) b0