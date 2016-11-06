module Regexdo.Pcre.Match where

import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import qualified Text.Regex.Base.RegexLike as R (makeRegex)
import Regexdo.TypeDo
import Regexdo.Pcre.Option as O
import Regexdo.TypeRegex
import Data.ByteString

{- |
    see "Regexdo.Pcre.Result"
    for funs converting 'MatchArray' to something useful

    'match' returns the first occurrence - if any

    -}


class Match_ctr n h =>
            Match_cl n h where

   match::Needle n -> Haystack h -> Maybe MatchArray
   match r0 (Haystack b0) = R.matchOnce (r_ r0) b0

   matchTest::Needle n -> Haystack h -> Bool
   matchTest r0 (Haystack b0) = R.matchTest (r_ r0) b0

   matchAll::Needle n -> Haystack h -> [MatchArray]
   matchAll r0 (Haystack b0) = R.matchAll (r_ r0) b0




-- | tweak Regex with options
makeRegexOpts::Match_opt n =>
    [O.Comp] -> [O.Exec] -> Needle n -> Regex
makeRegexOpts comp0 exec0 (Needle pat0) = rx1
   where c1 = O.comp comp0
         e1 = O.exec exec0
         rx1 = R.makeRegexOpts c1 e1 pat0


{- |
    this instance accepts regex 'String'

    >>> matchTest (Needle "^ab") (Haystack "abc")

    True
-}

instance Match_cl String String
-- | this instance accepts regex 'String'
instance Match_cl String ByteString
-- | this instance accepts regex 'ByteString'
instance Match_cl ByteString ByteString
-- | this instance accepts regex 'ByteString'
instance Match_cl ByteString String
-- | this instance accepts 'Regex' made with 'makeRegexOpts'
instance Match_cl Regex String
-- | this instance accepts 'Regex' made with 'makeRegexOpts'
instance Match_cl Regex ByteString



instance Needle_ ByteString where
   r_ (Needle r0) = R.makeRegex r0

instance Needle_ String where
   r_ (Needle r0) = R.makeRegex r0

instance Needle_ Regex where
   r_ (Needle r0) = r0


-- | _ctr: constraint
type Match_ctr n h = (R.Extract h, Needle_ n, R.RegexLike Regex h)
type Match_opt n = R.RegexMaker Regex CompOption ExecOption n

class Needle_ r where
   r_::Needle r -> Regex
