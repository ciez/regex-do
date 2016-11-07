-- | see "Text.Regex.Base.RegexLike"
module Text.Regex.Do.Pcre.Match where

import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import qualified Text.Regex.Base.RegexLike as R (makeRegex)
import Text.Regex.Do.TypeDo
import Text.Regex.Do.Pcre.Option as O
import Text.Regex.Do.TypeRegex
import Data.ByteString


{- | see "Text.Regex.Do.Pcre.Result"
    to convert 'MatchArray' to something useful
    -}

class Rx_ n h =>
            Match n h where

   matchOnce::Pattern n -> Body h -> Maybe MatchArray
   matchOnce r0 (Body b0) = R.matchOnce (r_ r0) b0

   matchTest::Pattern n -> Body h -> Bool
   matchTest r0 (Body b0) = R.matchTest (r_ r0) b0

   matchAll::Pattern n -> Body h -> [MatchArray]
   matchAll r0 (Body b0) = R.matchAll (r_ r0) b0



-- | tweak Regex with options
makeRegexOpts::Opt_ n =>
    [O.Comp] -> [O.Exec] -> Pattern n -> Regex
makeRegexOpts comp0 exec0 (Pattern pat0) = rx1
   where c1 = O.comp comp0
         e1 = O.exec exec0
         rx1 = R.makeRegexOpts c1 e1 pat0


{- | accepts regex 'String'

    >>> matchTest (Pattern "^ab") (Body "abc")

    True
-}
instance Match String String
-- | accepts regex 'String'
instance Match String ByteString
-- | accepts regex 'ByteString'
instance Match ByteString ByteString
-- | accepts regex 'ByteString'
instance Match ByteString String
-- | accepts 'Regex' made with 'makeRegexOpts'
instance Match Regex String
-- | accepts 'Regex' made with 'makeRegexOpts'
instance Match Regex ByteString




class Regex_ r where
   r_::Pattern r -> Regex


instance Regex_ ByteString where
   r_ (Pattern r0) = R.makeRegex r0

instance Regex_ String where
   r_ (Pattern r0) = R.makeRegex r0

instance Regex_ Regex where
   r_ (Pattern r0) = r0


type Rx_ n h = (R.Extract h, Regex_ n, R.RegexLike Regex h)
