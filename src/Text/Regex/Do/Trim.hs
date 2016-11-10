module Text.Regex.Do.Trim where

import Text.Regex.Do.Type.Do
import Data.Char(isSpace)
import qualified Data.ByteString as B
import Text.Regex.Do.Convert
import Text.Regex.Do.Pcre.Replace

{- | removes leading and trailing spaces and tabs   -}

class Trim a where
    trim::a -> a


instance Trim B.ByteString where
    trim bs1 = replace [All] rx1 repl1 $ Body bs1
       where repl1 = Replacement B.empty
             rx1 = rxFn "(^[\\s\\t]+)|([\\s\\t]+$)"
             rxFn = Pattern . toByteString

instance  Trim String where
    trim = f . f
       where f = reverse . dropWhile isSpace
