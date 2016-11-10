-- | __internal__ module, exposed only to show the instances
module Text.Regex.Do.Pcre.Matchf where

import Text.Regex.Do.Type.Do
import Text.Regex.Do.Type.Reexport
import qualified Text.Regex.Do.Pcre.Result as R
import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import Text.Regex.Do.Type.Regex_


class (Rx_ a b, Functor f) => Matchf f a b where
   marray_::Pattern a -> Body b -> f MatchArray
   poslen_::Pattern a -> Body b -> f [PosLen]
   poslen_ r0 b0 = R.poslen $ marray_ r0 b0

instance Rx_ a b => Matchf Maybe a b where
   marray_ p0 (Body b0) = R.matchOnce (r_ p0) b0

instance Rx_ a b => Matchf [] a b where
   marray_ p0 (Body b0) = R.matchAll (r_ p0) b0


once::Rx_ a b => Pattern a -> Body b -> [b]      -- ^ matched content
once p0 b0 = maybe [] id $ R.allMatches b0 $ marray_ p0 b0

all::Rx_ a b => Pattern a -> Body b -> [[b]]       -- ^ matched content
all p0 b0 = R.allMatches b0 $ marray_ p0 b0
