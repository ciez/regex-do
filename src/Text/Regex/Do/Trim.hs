module Text.Regex.Do.Trim where

import Text.Regex.Do.Type.Do
import Data.Char(isSpace)
import qualified Data.ByteString as B
import Text.Regex.Do.Convert
import Text.Regex.Do.Pcre.Ascii.Replace
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Type.Regex
import Text.Regex.Do.Pcre.Option


{- | removes leading and trailing spaces and tabs   -}

class Trim a where
    trim::a -> a


instance Trim B.ByteString where
    trim bs1 = replace (All rx2) repl1 $ Body bs1
       where repl1 = Replacement B.empty
             rx1 = "(^[\\s\\t]+)|([\\s\\t]+$)"
             rx2 = let p1 = Pattern $ toByteString rx1
                        in makeRegexOpt' p1 [Blank] []


instance  Trim String where
    trim = f . f
       where f = reverse . dropWhile isSpace
