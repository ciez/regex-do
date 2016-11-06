module Regexdo.Trim where

import Regexdo.TypeDo
import Data.Char(isSpace)
import qualified Data.ByteString as B
import Regexdo.Convert
import Regexdo.Pcre.Replace

{- | removes leading and trailing spaces and tabs   -}

class Trim a where
    trim::a -> a


instance Trim B.ByteString where
    trim bs1 = replace [All] (rx1,repl) $ Haystack bs1
       where repl = Replacement B.empty
             rx1 = rxFn "(^[\\s\\t]+)|([\\s\\t]+$)"
             rxFn = Needle . toByteString

instance  Trim String where
    trim = f . f
       where f = reverse . dropWhile isSpace
