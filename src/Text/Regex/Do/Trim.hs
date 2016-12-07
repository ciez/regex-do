module Text.Regex.Do.Trim where

import Data.Char(isSpace)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Text.Regex.Do.Type.Convert
import Text.Regex.Do.Replace.Latin
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Match.Regex
import Text.Regex.Do.Match.Option


{- | removes leading and trailing spaces and tabs   -}

class Trim a where
    trim::a -> a


instance Trim B.ByteString where
    trim bs1 = replace (All rx3) repl1 bs1
       where repl1 = B.empty
             rx1 = "(^[\\s\\t]+)|([\\s\\t]+$)"
             rx2 = toByteString rx1
             Right rx3 = makeRegexOpt rx2 [Blank] []  


instance Trim String where
    trim = f . f
       where f = reverse . dropWhile isSpace


instance Trim T.Text where
    trim = T.strip
-- ^ see 'T.strip'