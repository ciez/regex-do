module Text.Regex.Do.Convert where

import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import Data.ByteString as B
import Text.Regex.Do.Type.Do
import Text.Regex.Base.RegexLike as R
import Data.Array as A
import Prelude as P


-- | both Ascii and Utf8
toByteString::String -> ByteString
toByteString = E.encodeUtf8 . T.pack

toByteString'::String -> Utf8_ ByteString
toByteString' = Utf8_ . toByteString

-- | both Ascii and Utf8
toString::ByteString -> String
toString = T.unpack . E.decodeUtf8



class ToArray a where
    toArray::a -> MatchArray

instance ToArray MatchArray where
    toArray = id

instance ToArray [PosLen] where
    toArray [] = listArray (0,0) []
    toArray lpl0 = listArray (1, P.length lpl0) lpl0
