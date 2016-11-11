module Text.Regex.Do.Type.MatchHint where

newtype Test a = Test a     -- ^ test: does body match pattern?
newtype Once a = Once a     -- ^ values
newtype All a = All a       -- ^ values
newtype PosLen' a = PosLen' a   -- ^ once
newtype PosLen_ a = PosLen_ a   -- ^ all


class Hint hint where
    unhint::hint a -> a


instance Hint Test where unhint (Test a0) = a0
instance Hint Once where unhint (Once a0) = a0
instance Hint All where unhint (All a0) = a0
instance Hint PosLen' where unhint (PosLen' a0) = a0
instance Hint PosLen_ where unhint (PosLen_ a0) = a0