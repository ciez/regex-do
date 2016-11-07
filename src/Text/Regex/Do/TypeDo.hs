module Text.Regex.Do.TypeDo where

import Text.Regex.Base.RegexLike as R
import Text.Regex.Do.TypeRegex


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
