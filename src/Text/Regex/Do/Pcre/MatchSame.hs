module Text.Regex.Do.Pcre.MatchSame where

import Text.Regex.Do.Type.Do
import Text.Regex.PCRE.Wrap()
import Text.Regex.Do.Pcre.Match
import Data.ByteString


{- | picks 'Match' instance where 'Pattern' and 'Body' are of the same type

    specify either 'Pattern' or 'Body' + 'out' types

    handy when working with 'OverloadedStrings'

    >>> ("^all"::String) -~ "all the time"::[String]

    \["all"\]     -}
class MatchSame a out where
    match'::Match a a out => Pattern a -> Body a -> out
    match' = match
    (-~)::Match a a out => a  -- ^ pattern
                -> a          -- ^ body
                -> out        -- ^ \- in ('-~') is the minus sign
    (-~) = (=~)


instance MatchSame String Bool
instance MatchSame String [String]
instance MatchSame String [[String]]
instance MatchSame String [PosLen]
instance MatchSame String [[PosLen]]

instance MatchSame ByteString Bool
instance MatchSame ByteString [ByteString]
instance MatchSame ByteString [[ByteString]]
instance MatchSame ByteString [PosLen]
instance MatchSame ByteString [[PosLen]]
