-- | see "Text.Regex.Do.Pcre.Ascii.MatchHint" about MatchHint vs Match


module Text.Regex.Do.Pcre.Utf8.MatchHint
    (MatchHint(..),
    (=~)) where

import Text.Regex.Do.Type.Do hiding (Once,All)
import Text.Regex.PCRE.Wrap()
import qualified Text.Regex.Do.Pcre.Utf8.Match as M
import Text.Regex.Do.Type.MatchHint
import Data.ByteString


{- | * hint: 'Once', 'All', 'Test', 'PosLen'', 'PosLen_'
    * a: 'String', 'ByteString'
    * enc: 'Utf8_'

    picks 'M.Match' instance where 'Pattern' and 'Body' are of the same type

    'Hint' and inferrable 'Pattern' or 'Body' type determine the instance

    handy when working with 'OverloadedStrings', in other cases when compiler needs a hint  -}
class (Hint hint, M.Match enc a a (F hint a)) =>
    MatchHint hint enc a where
    type F hint a
    match::hint (Pattern (enc a)) -> Body (enc a) -> F hint a
    match h0 = M.match $ unhint h0


(=~)::MatchHint hint Utf8_ a =>
    hint a              -- ^ hint pattern
        -> a            -- ^ body
        -> F hint a     -- ^ type defined by the instance, determined by the hint
(=~) h0 = (M.=~) $ unhint h0


instance MatchHint Test Utf8_ String where
    type F Test String = Bool


instance MatchHint PosLen' Utf8_ String where
    type F PosLen' String = [PosLen]

{- ^ >>> PosLen' ("и"::String) =~ "бывает и хуже"

    \[(13,2)\]      -}

instance MatchHint PosLen_ Utf8_ String where
    type F PosLen_ String = [[PosLen]]

instance MatchHint Once Utf8_ String where
    type F Once String = [String]

{- ^ >>> Once ("^all"::String) =~ "all the time"

    \["all"\]       -}

instance MatchHint All Utf8_ String where
    type F All String = [[String]]

instance MatchHint Test Utf8_ ByteString where
    type F Test ByteString = Bool

{- ^ >>> Test (toByteString "в") =~ toByteString "тихо в лесу"

    True    -}

instance MatchHint PosLen' Utf8_ ByteString where
    type F PosLen' ByteString = [PosLen]

instance MatchHint PosLen_ Utf8_ ByteString where
    type F PosLen_ ByteString = [[PosLen]]

instance MatchHint Once Utf8_ ByteString where
    type F Once ByteString = [ByteString]

instance MatchHint All Utf8_ ByteString where
    type F All ByteString = [[ByteString]]
