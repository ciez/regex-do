module Text.Regex.Do.Type.Internal where


class Hint hint where
    unhint::hint a -> a
    hint::a -> hint a



instance Hint Test where
    unhint (Test a0) = a0
    hint = Test


newtype Test a = Test a     -- ^ test: does body match pattern?



-- | Haystack
data Body b = Body b deriving (Functor)                -- Bs, String
instance Applicative Body where
    pure p0 = Body p0
    (<*>) (Body f0) (Body a0) = Body $ f0 a0
