module Text.Regex.Do.Type.MatchHint where

import Text.Regex.Do.Type.Internal


newtype Once a = Once a     -- ^ replace once
        deriving (Functor)

newtype All a = All a        -- ^ replace all
        deriving (Functor)


instance Hint Once where
    unhint (Once a0) = a0
    hint = Once

instance Hint All where
    unhint (All a0) = a0
    hint = All


instance Applicative Once where
    pure p0 = Once p0
    (<*>) (Once f0) (Once a0) = Once $ f0 a0


instance Applicative All where
    pure p0 = All p0
    (<*>) (All f0) (All a0) = All $ f0 a0
