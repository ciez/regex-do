module Text.Regex.Do.Convert where

import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import Data.ByteString as B

-- | both Ascii and Utf8
toByteString::String -> ByteString
toByteString = E.encodeUtf8 . T.pack

-- | both Ascii and Utf8
toString::ByteString -> String
toString = T.unpack . E.decodeUtf8