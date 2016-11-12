module Text.Regex.Do.Type.MatchHint where

newtype Test a = Test a     -- ^ test: does body match pattern?
newtype Once a = Once a     -- ^ values
        deriving (Functor)

newtype All a = All a        -- ^ values
        deriving (Functor)

newtype PosLen' a = PosLen' a   -- ^ once
newtype PosLen_ a = PosLen_ a   -- ^ all


class Hint hint where
    unhint::hint a -> a
    hint::a -> hint a


instance Hint Test where
    unhint (Test a0) = a0
    hint = Test

instance Hint Once where
    unhint (Once a0) = a0
    hint = Once

instance Hint All where
    unhint (All a0) = a0
    hint = All

instance Hint PosLen' where
    unhint (PosLen' a0) = a0
    hint = PosLen'

instance Hint PosLen_ where
    unhint (PosLen_ a0) = a0
    hint = PosLen_


instance Applicative Once where
    pure p0 = Once p0
    (<*>) (Once f0) (Once a0) = Once $ f0 a0


instance Applicative All where
    pure p0 = All p0
    (<*>) (All f0) (All a0) = All $ f0 a0
