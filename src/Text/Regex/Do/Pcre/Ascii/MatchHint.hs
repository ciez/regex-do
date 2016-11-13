{- | this module uses
    <https://cdepillabout.github.io/haskell-type-families-presentation/#/ TypeFamilies>

    this module is similar to "Text.Regex.Do.Pcre.Ascii.Match". The differences are:

    "Text.Regex.Do.Pcre.Ascii.Match" is more flexible:
        accepts 'Pattern' Regex,
        accepts 'Pattern' and 'Body' of different types

    "Text.Regex.Do.Pcre.Ascii.Match" needs to infer result type

    in this module the result type is determined by the hint
    -}

module Text.Regex.Do.Pcre.Ascii.MatchHint where

import Text.Regex.Do.Type.Do hiding (Once,All)
import Text.Regex.PCRE.Wrap()
import qualified Text.Regex.Do.Pcre.Ascii.Match as M
import Text.Regex.Do.Type.MatchHint
import Data.ByteString


{- | picks 'M.Match' instance where 'Pattern' and 'Body' are of the same type

    'Hint' and inferrable 'Pattern' or 'Body' type determine the instance

    handy when working with 'OverloadedStrings', in other cases when compiler needs a hint  -}



class (Hint hint, M.Match a a (F hint a)) =>
    MatchHint hint a where
    type F hint a
    match::hint (Pattern a) -> Body a -> F hint a
    match h0 = M.match $ unhint h0

(=~)::MatchHint hint a =>
        hint a  -- ^ hint & pattern
        -> a          -- ^ body
        -> F hint a   -- ^ type defined by the instance, determined by the hint
(=~) h0 = (M.=~) $ unhint h0


instance MatchHint Test String where
    type F Test String = Bool


instance MatchHint PosLen' String where
    type F PosLen' String = [PosLen]

{- ^ >>> PosLen' ("и"::String) =~ "бывает и хуже"

    \[(13,2)\]      -}

instance MatchHint PosLen_ String where
    type F PosLen_ String = [[PosLen]]

instance MatchHint Once String where
    type F Once String = [String]

{- ^ >>> Once ("^all"::String) =~ "all the time"

    \["all"\]       -}

instance MatchHint All String where
    type F All String = [[String]]

instance MatchHint Test ByteString where
    type F Test ByteString = Bool

{- ^ >>> Test ("в"::ByteString) =~ "тихо в лесу"

    True    -}

instance MatchHint PosLen' ByteString where
    type F PosLen' ByteString = [PosLen]

instance MatchHint PosLen_ ByteString where
    type F PosLen_ ByteString = [[PosLen]]

instance MatchHint Once ByteString where
    type F Once ByteString = [ByteString]

instance MatchHint All ByteString where
    type F All ByteString = [[ByteString]]
