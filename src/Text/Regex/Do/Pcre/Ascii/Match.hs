{- | although sometimes funs in Ascii modules work with non-ascii text
    (as some examples show),
    for reliable results with Utf8 pattern or body,
    use "Text.Regex.Do.Pcre.Utf8.Match"

    see also "Text.Regex.Base.RegexLike" and "Text.Regex.Do.Pcre.Ascii.MatchHint" -}

module Text.Regex.Do.Pcre.Ascii.Match
    (Match(..),
    (=~),
    R.extract   -- | 'extract' is reexport from "Text.Regex.Base.RegexLike"
    ) where

import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Pcre.Matchf as F
import Text.Regex.PCRE.Wrap()
import Text.Regex.Do.Type.Regex
import Text.Regex.Do.Type.MatchHint


{- | short version of 'match': arg without newtypes

    is borrowed from "Text.Regex.PCRE.Wrap"    -}
(=~)::Match a b out =>
    a     -- ^ pattern
    -> b    -- ^ body
    -> out
(=~) p0 b0 = match (Pattern p0) (Body b0)



{- | * a:  'String', 'ByteString', 'Regex'
    * b:  'String', 'ByteString'
    * out:

        * ['String'], [['String']]
        * ['ByteString'], [['ByteString']]
        * 'Bool'
        * ['PosLen'], [['PosLen']]


    precompiled Regex may be used as pattern too. see "Text.Regex.Do.Pcre.Utf8.Match"

    See also "Text.Regex.Do.Pcre.Ascii.MatchHint"

    to catch regex construction __errors__, precompile 'Regex' with 'makeRegexM' or 'makeRegexOptM'     -}

class Match a b out where
    match::Pattern a -> Body b -> out




-- | match once
instance Rx_ a b => Match a b [b] where
    match p0 = once (makeRegex' p0)
{- ^  >>> "^all" =~ "all the time"::[String]

     \["all"\]      -}
instance Rx_ a b => Match a b Bool where
    match p0 (Body b0) = R.matchTest (makeRegex p0) b0
{- ^ test

    >>> "chilly" =~ "it's chilly inside, chilly outside"::Bool

    True    -}

-- | match all
instance Rx_ a b => Match a b [[b]] where
    match p0 = F.all (makeRegex' p0)
{- ^  >>> "chilly" =~ "it's chilly inside, chilly outside"::[[ByteString]]

     \[["chilly"\],\["chilly"\]]        -}

-- | match once
instance Rx_ a b => Match a b [PosLen] where
    match p0 b0 = maybe [] id $ poslen_ (Once $ makeRegex' p0) b0
{- ^ >>> "à" =~ "tourner à gauche"::[PosLen]

     \[(8,2)\]      -}

-- | match all
instance Rx_ a b => Match a b [[PosLen]] where
    match p0 = poslen_ $ All $ makeRegex' p0

