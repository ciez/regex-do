{- | this module uses
    <https://cdepillabout.github.io/haskell-type-families-presentation/#/ TypeFamilies>
    -}

{-# LANGUAGE TypeFamilies #-}
module Text.Regex.Do.Pcre.MatchHint where

import Text.Regex.Do.Type.Do hiding (Once,All)
import Text.Regex.PCRE.Wrap()
import qualified Text.Regex.Do.Pcre.Match as M
import Text.Regex.Do.Type.MatchHint
import Data.ByteString


{- | picks 'M.Match' instance where 'Pattern' and 'Body' are of the same type

    'Hint' and inferrable 'Pattern' or 'Body' type determine the instance

    handy when working with 'OverloadedStrings', in other cases when compiler needs a hint

    >>> Once ("^all"::String) =~ "all the time"

    \["all"\]

    >>> PosLen' ("и"::String) =~ "бывает и хуже"

    \[(13,2)\]      -}
class (Hint hint, M.Match a a (F hint a)) =>
    MatchHint hint a where
    type F hint a
    match::hint (Pattern a) -> Body a -> F hint a
    match h0 = M.match $ unhint h0

    (=~)::hint a  -- ^ pattern
            -> a          -- ^ body
            -> F hint a        -- ^ \- in ('-~') is the minus sign
    (=~) h0 = (M.=~) $ unhint h0


instance MatchHint Test String where
    type F Test String = Bool

instance MatchHint PosLen' String where
    type F PosLen' String = [PosLen]

instance MatchHint PosLen_ String where
    type F PosLen_ String = [[PosLen]]

instance MatchHint Once String where
    type F Once String = [String]

instance MatchHint All String where
    type F All String = [[String]]

instance MatchHint Test ByteString where
    type F Test ByteString = Bool

instance MatchHint PosLen' ByteString where
    type F PosLen' ByteString = [PosLen]

instance MatchHint PosLen_ ByteString where
    type F PosLen_ ByteString = [[PosLen]]

instance MatchHint Once ByteString where
    type F Once ByteString = [ByteString]

instance MatchHint All ByteString where
    type F All ByteString = [[ByteString]]