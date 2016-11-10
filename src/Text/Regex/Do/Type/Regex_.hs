module Text.Regex.Do.Type.Regex_ where

import Text.Regex.Base.RegexLike as R
import Text.Regex.Do.Type.Reexport
import Data.ByteString
import Text.Regex.Do.Type.Do


class Regex_ a where
   r_::Pattern a -> Regex

instance Regex_ ByteString where
   r_ (Pattern p0) = R.makeRegex p0

instance Regex_ String where
   r_ (Pattern p0) = R.makeRegex p0

instance Regex_ Regex where
   r_ (Pattern p0) = p0


type Rx_ a b = (Regex_ a, R.Extract b, R.RegexLike Regex b)
