module Text.Regex.Do.Match.Regex
    (Regex(..),
    E(..)) where

import qualified Text.Regex.Base.RegexLike as R
import qualified Text.Regex.Do.Type.Reexport as R
import Control.Monad.Fail
import Data.ByteString
import Text.Regex.Do.Type.Do hiding (Regex)
import Text.Regex.Do.Match.Option
import Data.List as L


class Regex a where
   makeRegex::a -> E R.Regex
   makeRegexOpt::a -> [Comp] -> [Exec] -> E R.Regex


instance Regex ByteString where
   makeRegex p0 = rre $ R.makeRegexM p0 
   makeRegexOpt p0 o0 e0 = rre $ makeRegexOptsM p0 o0 e0 


instance Regex String where
   makeRegex p0 = rre $ R.makeRegexM p0
   makeRegexOpt p0 o0 e0 = rre $ makeRegexOptsM p0 o0 e0 


instance Regex R.Regex where
   makeRegex p0 = pure p0
   makeRegexOpt p0 _ _ = pure p0


makeRegexOptsM::(Monad m, Opt_ a, Control.Monad.Fail.MonadFail m) =>
    a -> [Comp] -> [Exec] ->
            m R.Regex
makeRegexOptsM pat0 comp0 exec0 = rx1
   where c1 = comp comp0
         e1 = exec exec0
         rx1 = R.makeRegexOptsM c1 e1 pat0


-- | internal type
type Opt_ a = R.RegexMaker R.Regex R.CompOption R.ExecOption a


rre::RegexResult a -> Either String a
rre (RegexResult e0) = either (Left . (L.intercalate " ")) Right e0


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

instance MonadFail RegexResult where
    fail :: String -> RegexResult a
    fail err0 = RegexResult $ Left [err0]