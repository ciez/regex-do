-- | see "Text.Regex.Base.RegexLike"
module Text.Regex.Do.Pcre.Match
    (Match(..),
    R.extract,   -- | 'extract' is reexport from "Text.Regex.Base.RegexLike"
    makeRegexOpts) where

import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import Text.Regex.Do.TypeDo
import Text.Regex.Do.Pcre.Option as O
import Text.Regex.Do.TypeRegex
import qualified Text.Regex.Do.Pcre.Result as R
import Text.Regex.Do.Pcre.Matchf


{- | 'match' covers all

    compiler looks up the appropriate function depending on the result type

    ('=~') is borrowed from "Text.Regex.Posix.Wrap",
    is a short version of 'match'. For those who put pattern & body in the right places.
    -}
class Match n h out where
    match::Pattern n -> Body h -> out
    (=~)::n -> h -> out
    (=~) n0 h0 = match (Pattern n0) (Body h0)


-- | match once
instance Rx_ n h => Match n h [h] where
    match = matchOnce
{- ^  >>> "^all" =~ "all the time"::[String]

     \["all"\]    -}
instance Rx_ n h => Match n h Bool where
    match = matchTest
{- ^ test

    >>> "в" =~ "тихо в лесу"::Bool

    True
    -}

-- | match all
instance Rx_ n h => Match n h [[h]] where
    match = matchAll
{- ^  >>> "well" =~ "all is well that ends well"::[[ByteString]]

     \[["well"\],\["well"\]]    -}

-- | match once
instance Rx_ n h => Match n h [PosLen] where
    match p0 b0 = maybe [] id $ poslen_ p0 b0
{- ^ >>> ("и"::String) =~ ("бывает и хуже"::String)::[PosLen]

     \[(13,2)\]     /Utf8/       -}

-- | match all
instance Rx_ n h => Match n h [[PosLen]] where
    match = poslen_


matchOnce::Rx_ n h => Pattern n -> Body h -> [h]      -- ^ matched content
matchOnce r0 b0 = maybe [] id $ R.allMatches b0 $ marray_ r0 b0

matchTest::Rx_ n h => Pattern n -> Body h -> Bool
matchTest r0 (Body b0) = R.matchTest (r_ r0) b0

matchAll::Rx_ n h => Pattern n -> Body h -> [[h]]       -- ^ matched content
matchAll r0 b0 = R.allMatches b0 $ marray_ r0 b0



-- | tweak Regex with options
makeRegexOpts::Opt_ n =>
    [O.Comp] -> [O.Exec] -> Pattern n -> Regex
makeRegexOpts comp0 exec0 (Pattern pat0) = rx1
   where c1 = O.comp comp0
         e1 = O.exec exec0
         rx1 = R.makeRegexOpts c1 e1 pat0
