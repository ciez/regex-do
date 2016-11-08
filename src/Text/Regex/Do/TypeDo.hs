module Text.Regex.Do.TypeDo where

import Text.Regex.Base.RegexLike as R
import Text.Regex.Do.TypeRegex
import Data.ByteString


-- pcre
type GroupReplacer a = (MatchArray -> ReplaceAcc a -> ReplaceAcc a) -- MatchArray -> acc -> acc


data ReplaceAcc a = ReplaceAcc {
    acc::a,   -- ^ Body with some replacements made
    pos_adj::Int    {- ^ position adjustment: group replacement length may differ from replaced text length -}
    }


-- | Needle
data Pattern n = Pattern n  deriving (Functor)          -- Bs, String, RegexPcre

-- | Haystack
data Body h = Body h deriving (Functor)                -- Bs, String

data Replacement r = Replacement r deriving (Functor)     --    Bs, String

-- | Offset, Length
type PosLen = (MatchOffset, MatchLength)


data ReplaceCase = Once     -- ^ may be omitted
                | All       -- ^ if both Once and All are passed, All prevails
                | Utf8
                | Multiline deriving Eq



type Opt_ n = R.RegexMaker Regex CompOption ExecOption n
type Rx_ n h = (R.Extract h, Regex_ n, R.RegexLike Regex h)


class Regex_ r where
   r_::Pattern r -> Regex

instance Regex_ ByteString where
   r_ (Pattern r0) = R.makeRegex r0

instance Regex_ String where
   r_ (Pattern r0) = R.makeRegex r0

instance Regex_ Regex where
   r_ (Pattern r0) = r0
