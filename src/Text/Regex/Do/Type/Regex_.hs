module Text.Regex.Do.Type.Regex_ where

import qualified Text.Regex.Base.RegexLike as R
import Text.Regex.Do.Type.Reexport
import Data.ByteString
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Pcre.Option


class Regex_ a where
   r_::Pattern a -> Regex
   ropt_::Pattern a -> [Comp] -> [Exec] -> Regex


instance Regex_ ByteString where
   r_ (Pattern p0) = R.makeRegex p0
   ropt_ = makeRegexOpts

instance Regex_ String where
   r_ (Pattern p0) = R.makeRegex p0
   ropt_ = makeRegexOpts

instance Regex_ Regex where
   r_ (Pattern p0) = p0
   ropt_ (Pattern p0) _ _ = p0


type Rx_ a b = (Regex_ a, R.Extract b, R.RegexLike Regex b)
type Opt_ a = R.RegexMaker Regex CompOption ExecOption a


-- | tweak Regex with options
makeRegexOpts::Opt_ a =>
    Pattern a ->
        [Comp] -> [Exec] ->
            Regex
makeRegexOpts (Pattern pat0) comp0 exec0 = rx1
   where c1 = comp comp0
         e1 = exec exec0
         rx1 = R.makeRegexOpts c1 e1 pat0
