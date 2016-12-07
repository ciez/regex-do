module Text.Regex.Do.Type.Do where

import Text.Regex.Base.RegexLike as R


-- | see "Text.Regex.Do.Replace.Open" 'defaultReplacer' for example implementation
newtype GroupReplacer b = GroupReplacer (MatchArray -> ReplaceAcc b -> ReplaceAcc b) -- MatchArray -> acc -> acc

data ReplaceAcc b = ReplaceAcc {
    acc::b,   -- ^ content with some replacements made
    pos_adj::Int    {- ^ position adjustment: group replacement length may differ from replaced text length -}
    }

instance Functor ReplaceAcc where
    fmap fn0 r0 = r0 { acc = fn0 $ acc r0 }



-- | Haystack
data Body b = Body b deriving (Functor)                -- Bs, String
instance Applicative Body where
    pure p0 = Body p0
    (<*>) (Body f0) (Body a0) = Body $ f0 a0


-- | Offset, Length
type PosLen = (MatchOffset, MatchLength)


{-| 'Left' String returns regex construction error -}
type E a = Either String a
