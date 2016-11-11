module Text.Regex.Do.Type.Do where

import Text.Regex.Base.RegexLike as R


-- | see "Text.Regex.Do.Pcre.ReplaceOpen" 'defaultReplacer' for example implementation
newtype GroupReplacer b = GroupReplacer (MatchArray -> ReplaceAcc b -> ReplaceAcc b) -- MatchArray -> acc -> acc


data ReplaceAcc b = ReplaceAcc {
    acc::b,   -- ^ content with some replacements made
    pos_adj::Int    {- ^ position adjustment: group replacement length may differ from replaced text length -}
    }


-- | Needle
data Pattern a = Pattern a  deriving (Functor)          -- Bs, String, RegexPcre

-- | Haystack
data Body b = Body b deriving (Functor)                -- Bs, String

data Replacement r = Replacement r deriving (Functor)     --    Bs, String

-- | Offset, Length
type PosLen = (MatchOffset, MatchLength)
