module Text.Regex.Do.Type.Regex
    (Regex(..),
    makeRegex',
    makeRegexOpt',
    RegexResult(..),
    Rx_, Opt_, Ro_) where

import qualified Text.Regex.Base.RegexLike as R
import qualified Text.Regex.Do.Type.Reexport as R
import Data.ByteString
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Pcre.Option


class Regex a where
   makeRegex::Pattern a -> R.Regex
   makeRegexM::Monad m => Pattern a -> m R.Regex
   makeRegexOpt::Pattern a -> [Comp] -> [Exec] -> R.Regex
   makeRegexOptM::Monad m => Pattern a -> [Comp] -> [Exec] -> m R.Regex

{- ^ monadic

        * 'makeRegexM'
        * 'makeRegexOptM'

        let catch regex construction errors

    for 'm' to catch errors, implement 'fail' in 'm'

    default 'm' implementation: 'RegexResult'

    ==== makeRegexM test case:

    >>>  it "RegexResult test case" $ do
            rx1 `shouldNotSatisfy` isok1
            rx2 `shouldSatisfy` isok1
        where rx1 = T.makeRegexM $ Pattern "[["::RegexResult R.Regex
              rx2 = T.makeRegexM $ Pattern "."::RegexResult R.Regex
              isok1 (RegexResult (Left e1)) = traceShow e1 False
              isok1 _ = True

    >>> instance Show (RegexResult R.Regex) where
            show _ = "bon"       -}

makeRegex'::Regex a => Pattern a -> Pattern R.Regex
makeRegex' = Pattern . makeRegex

makeRegexOpt'::Regex a => Pattern a -> [Comp] -> [Exec] -> Pattern R.Regex
makeRegexOpt' p0 c0 e0 = Pattern $ makeRegexOpt p0 c0 e0



instance Regex a => Regex (Utf8_ a) where
   makeRegex p0 = makeRegexOpt (Pattern val <*> p0) [Utf8] []
   makeRegexM p0 = makeRegexOptM (Pattern val <*> p0) [Utf8] []
   makeRegexOpt p0 c0 e0 = makeRegexOpt (Pattern val <*> p0) (Utf8:c0) e0
   makeRegexOptM p0 c0 e0 = makeRegexOptM (Pattern val <*> p0) (Utf8:c0) e0


instance Regex ByteString where
   makeRegex (Pattern p0) = R.makeRegex p0
   makeRegexM (Pattern p0) = R.makeRegexM p0
   makeRegexOpt = makeRegexOpts
   makeRegexOptM = makeRegexOptsM


instance Regex String where
   makeRegex (Pattern p0) = R.makeRegex p0
   makeRegexM (Pattern p0) = R.makeRegexM p0
   makeRegexOpt = makeRegexOpts
   makeRegexOptM = makeRegexOptsM


instance Regex R.Regex where
   makeRegex (Pattern p0) = p0
   makeRegexM (Pattern p0) = pure p0
   makeRegexOpt (Pattern p0) _ _ = p0
   makeRegexOptM (Pattern p0) _ _ = pure p0


-- | tweak Regex with options
makeRegexOpts::Opt_ a =>
    Pattern a ->
        [Comp] -> [Exec] ->
            R.Regex
makeRegexOpts (Pattern pat0) comp0 exec0 = rx1
   where c1 = comp comp0
         e1 = exec exec0
         rx1 = R.makeRegexOpts c1 e1 pat0


makeRegexOptsM::(Monad m, Opt_ a) =>
    Pattern a ->
        [Comp] -> [Exec] ->
            m R.Regex
makeRegexOptsM (Pattern pat0) comp0 exec0 = rx1
   where c1 = comp comp0
         e1 = exec exec0
         rx1 = R.makeRegexOptsM c1 e1 pat0



type Rx_ a b = (Regex a, R.Extract b, R.RegexLike R.Regex b)
type Opt_ a = R.RegexMaker R.Regex R.CompOption R.ExecOption a
type Ro_ rx = (Regex rx, Opt_ rx)


{- | catches regex construction __errors__  -}
newtype RegexResult a = RegexResult (Either [String] a) deriving (Functor)

instance Applicative RegexResult where
    pure = RegexResult . Right
    (<*>) (RegexResult (Left e1)) (RegexResult (Left e2)) = RegexResult $ Left $ e1 ++ e2
    (<*>) (RegexResult (Right fn0)) (RegexResult (Left e1)) = RegexResult $ Left e1
    (<*>) (RegexResult (Left e1)) (RegexResult (Right r1)) = RegexResult $ Left e1
    (<*>) (RegexResult (Right fn0)) (RegexResult (Right a0)) = pure $ fn0 a0

instance Monad RegexResult where
    (>>=) (RegexResult (Left e1)) fn0 = RegexResult $ Left e1
    (>>=) (RegexResult (Right a0)) fn0 = fn0 a0
    fail s0 = RegexResult $ Left [s0]
