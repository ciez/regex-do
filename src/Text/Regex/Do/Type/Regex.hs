module Text.Regex.Do.Type.Regex
    (Regex(..),
    makeRegex',
    makeRegexOpt',
    Rx_, Opt_, Ro_) where

import qualified Text.Regex.Base.RegexLike as R
import qualified Text.Regex.Do.Type.Reexport as R
import Data.ByteString
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Pcre.Option


class Regex a where
   makeRegex::Pattern a -> R.Regex
   makeRegexOpt::Pattern a -> [Comp] -> [Exec] -> R.Regex


makeRegex'::Regex a => Pattern a -> Pattern R.Regex
makeRegex' = Pattern . makeRegex

makeRegexOpt'::Regex a => Pattern a -> [Comp] -> [Exec] -> Pattern R.Regex
makeRegexOpt' p0 c0 e0 = Pattern $ makeRegexOpt p0 c0 e0



instance Regex a => Regex (Utf8_ a) where
   makeRegex p0 = makeRegexOpt (Pattern val <*> p0) [Utf8] []
   makeRegexOpt p0 c0 e0 = makeRegexOpt (Pattern val <*> p0) (Utf8:c0) e0


instance Regex ByteString where
   makeRegex (Pattern p0) = R.makeRegex p0
   makeRegexOpt = makeRegexOpts


instance Regex String where
   makeRegex (Pattern p0) = R.makeRegex p0
   makeRegexOpt = makeRegexOpts


instance Regex R.Regex where
   makeRegex (Pattern p0) = p0
   makeRegexOpt (Pattern p0) _ _ = p0


-- | tweak Regex with options
makeRegexOpts::Opt_ a =>
    Pattern a ->
        [Comp] -> [Exec] ->
            R.Regex
makeRegexOpts (Pattern pat0) comp0 exec0 = rx1
   where c1 = comp comp0
         e1 = exec exec0
         rx1 = R.makeRegexOpts c1 e1 pat0


type Rx_ a b = (Regex a, R.Extract b, R.RegexLike R.Regex b)
type Opt_ a = R.RegexMaker R.Regex R.CompOption R.ExecOption a
type Ro_ rx = (Regex rx, Opt_ rx)
