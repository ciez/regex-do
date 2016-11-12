module Text.Regex.Do.Type.Do where

import Text.Regex.Base.RegexLike as R


-- | see "Text.Regex.Do.Pcre.ReplaceOpen" 'defaultReplacer' for example implementation
newtype GroupReplacer b = GroupReplacer (MatchArray -> ReplaceAcc b -> ReplaceAcc b) -- MatchArray -> acc -> acc

data ReplaceAcc b = ReplaceAcc {
    acc::b,   -- ^ content with some replacements made
    pos_adj::Int    {- ^ position adjustment: group replacement length may differ from replaced text length -}
    }

instance Functor ReplaceAcc where
    fmap fn0 r0 = r0 { acc = fn0 $ acc r0 }



-- | Needle
data Pattern a = Pattern a  deriving (Functor)          -- Bs, String, RegexPcre
instance Applicative Pattern where
    pure p0 = Pattern p0
    (<*>) (Pattern f0) (Pattern a0) = Pattern $ f0 a0


-- | Haystack
data Body b = Body b deriving (Functor)                -- Bs, String
instance Applicative Body where
    pure p0 = Body p0
    (<*>) (Body f0) (Body a0) = Body $ f0 a0


data Replacement r = Replacement r deriving (Functor)     --    Bs, String
instance Applicative Replacement where
    pure p0 = Replacement p0
    (<*>) (Replacement f0) (Replacement a0) = Replacement $ f0 a0

-- | Offset, Length
type PosLen = (MatchOffset, MatchLength)



newtype Utf8_ a = Utf8_ a       -- ^ values
        deriving (Functor,Eq,Ord)

instance Applicative Utf8_ where
    pure p0 = Utf8_ p0
    (<*>) (Utf8_ f0) (Utf8_ a0) = Utf8_ $ f0 a0


-- ^ does not do any codec. Plain wrap / unwrap newtype
class Enc enc where
    val::enc a -> a
    enc::a -> enc a

-- ^ does not do any codec. Plain wrap / unwrap newtype
class Enc' f enc where
    val'::f (enc a) -> f a
    enc'::f a -> f (enc a)


instance Enc Utf8_ where
    val (Utf8_ v0) = v0
    enc = Utf8_


instance Enc enc => Enc' GroupReplacer enc where
    val' (GroupReplacer fn0) = GroupReplacer $
        \ma1 acc1 -> val <$> (fn0 ma1 $ enc <$> acc1)
    enc' (GroupReplacer fn0) = GroupReplacer $
        \ma1 acc1 -> enc <$> (fn0 ma1 $ val <$> acc1)
