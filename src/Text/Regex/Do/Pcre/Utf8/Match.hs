{- | see "Text.Regex.Base.RegexLike" and "Text.Regex.Do.Pcre.Utf8.MatchHint"

    similar to "Text.Regex.Do.Pcre.Ascii.Match"

    'Pattern' & 'Body' are wrapped in 'Utf8_' encoding tag.
    This tag adds clarity, prevents calling Ascii functions by mistake.

    __'toByteString''__ converts String to 'Utf8_' 'ByteString'     -}
module Text.Regex.Do.Pcre.Utf8.Match
    (Match(..),
    (=~),
    R.extract   -- | 'extract' is reexport from "Text.Regex.Base.RegexLike"
    ) where

import Text.Regex.Do.Type.Reexport as R
import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Pcre.Matchf as F
import Text.Regex.PCRE.Wrap()
import Text.Regex.Do.Type.Regex as T
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Pcre.Option
import Text.Regex.Do.Convert
import Data.ByteString



class Match enc a b out where
    match::Pattern (enc a) -> Body (enc b) -> out

(=~)::Match Utf8_ a b out =>
    a     -- ^ pattern
    -> b    -- ^ body
    -> out
(=~) p0 b0 = match (Pattern $ Utf8_ p0) (Body $ Utf8_ b0)


{- | match once

    ==== precompiled regex as pattern

 >>> let rx1 = makeRegexOpt' (Pattern $ toByteString' "左") [] []      --  add options as needed
         rx2 = Utf8_ <$> rx1
         m1 = U.match rx2 (Body $ toByteString' "100メートル左折後、左")::[ByteString]
      m1 `shouldBe` [toByteString "左"]        -}

instance Rx_ a b => Match Utf8_ a b [b] where
    match p0 b0 = once (mr_ p0) $ val <$> b0



instance Rx_ a b => Match Utf8_ a b Bool where
    match p0 (Body b0) = R.matchTest (makeRegexOpt p0 [Utf8] []) $ val b0
{- ^ test. Note that a and b may be different types e.g. 'ByteString' and 'String'

    >>>  toByteString "в" =~ ("тихо в лесу"::String)::Bool

    True        -}

-- | match all
instance Rx_ a b => Match Utf8_ a b [[b]] where
    match p0 b0 = F.all (mr_ p0) $ val <$> b0
{- ^  >>> ("well"::String) =~ ("all is well that ends well"::String)::[[String]]

     \[["well"\],\["well"\]]        -}

-- | match once
instance Rx_ a b => Match Utf8_ a b [PosLen] where
    match p0 b0 = maybe [] id $ poslen_ (Once $ mr_ p0) $ val <$> b0
{- ^ >>> ("и"::String) =~ ("бывает и хуже"::String)::[PosLen]

     \[(13,2)\]     -}

-- | match all
instance Rx_ a b => Match Utf8_ a b [[PosLen]] where
    match p0 b0 = poslen_ (All $ mr_ p0) $ val <$> b0


mr_::T.Regex a => Pattern a -> Pattern R.Regex
mr_ p0 = makeRegexOpt' p0 [Utf8] []


dum_::String -> Utf8_ ByteString
dum_ = toByteString'