{- | although sometimes funs in Ascii modules work with non-ascii text
    (as some examples show),
    for reliable results with Utf8 pattern or body,
    use "Text.Regex.Do.Pcre.Utf8.Match"

    see also "Text.Regex.Base.RegexLike" and "Text.Regex.Do.Pcre.Ascii.MatchHint" -}

module Text.Regex.Do.Pcre.Ascii.Match
    (Match(..),
    R.extract   -- | 'extract' is reexport from "Text.Regex.Base.RegexLike"
    ) where

import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Pcre.Matchf as F
import Text.Regex.PCRE.Wrap()
import Text.Regex.Do.Type.Regex
import Text.Regex.Do.Type.MatchHint


{- | 'match' covers all result types

    compiler looks up the appropriate function depending on the result type

    '=~' is borrowed from "Text.Regex.PCRE.Wrap",
    is a short version of 'match'

    See also "Text.Regex.Do.Pcre.Ascii.MatchHint"       -}

class Match a b out where
    match::Pattern a -> Body b -> out
    (=~)::a     -- ^ pattern
        -> b    -- ^ body
        -> out
    (=~) p0 b0 = match (Pattern p0) (Body b0)


-- | match once
instance Rx_ a b => Match a b [b] where
    match p0 = once (makeRegex' p0)
{- ^  >>> "^all" =~ "all the time"::[String]

     \["all"\]      -}
instance Rx_ a b => Match a b Bool where
    match p0 (Body b0) = R.matchTest (makeRegex p0) b0
{- ^ test

    >>> "в" =~ "тихо в лесу"::Bool

    True    -}

-- | match all
instance Rx_ a b => Match a b [[b]] where
    match p0 = F.all (makeRegex' p0)
{- ^  >>> "well" =~ "all is well that ends well"::[[ByteString]]

     \[["well"\],\["well"\]]        -}

-- | match once
instance Rx_ a b => Match a b [PosLen] where
    match p0 b0 = maybe [] id $ poslen_ (Once $ makeRegex' p0) b0
{- ^ >>> "à" =~ "tourner à gauche"::[PosLen]

     \[(8,2)\]      -}

-- | match all
instance Rx_ a b => Match a b [[PosLen]] where
    match p0 = poslen_ $ All $ makeRegex' p0

