-- | see "Text.Regex.Base.RegexLike" and "Text.Regex.Do.Pcre.MatchHint"
module Text.Regex.Do.Pcre.Match
    (Match(..),
    R.extract,   -- | 'extract' is reexport from "Text.Regex.Base.RegexLike"
    makeRegexOpts) where

import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Pcre.Matchf as F
import Text.Regex.PCRE.Wrap()
import Text.Regex.Do.Type.Regex_


{- | 'match' covers all result types

    compiler looks up the appropriate function depending on the result type

    '=~' is borrowed from "Text.Regex.PCRE.Wrap",
    is a short version of 'match'

    See also "Text.Regex.Do.Pcre.MatchHint"       -}

class Match a b out where
    match::Pattern a -> Body b -> out
    (=~)::a     -- ^ pattern
        -> b    -- ^ body
        -> out
    (=~) p0 b0 = match (Pattern p0) (Body b0)


-- | match once
instance Rx_ a b => Match a b [b] where
    match = once
{- ^  >>> "^all" =~ "all the time"::[String]

     \["all"\]

     "Text.Regex.Do.Pcre.MatchHint"     -}
instance Rx_ a b => Match a b Bool where
    match p0 (Body b0) = R.matchTest (r_ p0) b0
{- ^ test

    >>> "в" =~ "тихо в лесу"::Bool

    True

    "Text.Regex.Do.Pcre.MatchHint"      -}

-- | match all
instance Rx_ a b => Match a b [[b]] where
    match = F.all
{- ^  >>> "well" =~ "all is well that ends well"::[[ByteString]]

     \[["well"\],\["well"\]]

     "Text.Regex.Do.Pcre.MatchHint"    -}

-- | match once
instance Rx_ a b => Match a b [PosLen] where
    match p0 b0 = maybe [] id $ poslen_ p0 b0
{- ^ >>> "и" =~ "бывает и хуже"::[PosLen]

     \[(13,2)\]

     /Utf8/

     "Text.Regex.Do.Pcre.MatchHint"     -}

-- | match all
instance Rx_ a b => Match a b [[PosLen]] where
    match = poslen_

