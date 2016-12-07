{- | see "Data.ByteString.Search" package

    break, split ops on 'ByteString'

    regex is treated as ordinary String
    
    break & split are now '/', '-/', '/-' 
    
    replace moved to "Text.Regex.Do.Replace.Fast"
    -}
module Text.Regex.Do.Split
    (Split(..),
    SplitFront(..),
    SplitEnd(..),
    T,L
    ) where

import qualified Data.ByteString.Search as S
import Data.ByteString as B hiding (break, breakEnd, split)
import Prelude hiding (break,(/))
import Text.Regex.Do.Match.Matchf


-- | Break result: tuple
type T = (B.ByteString,B.ByteString)

-- | Split result: list
type L = [B.ByteString] 



{- | slices 'ByteString'. drops needle
    
    to avoid clash with 'Prelude':
    
    @import Prelude hiding((/))@       

    or qualify '/' with alias e.g. (assuming this module is imported with S alias):
       
    @S./@           
    
    body -> pattern -> result
    -}
class Split out where
    (/)::ByteString -> ByteString -> out


instance Split (ByteString,ByteString) where
    (/) body0 pat0 =  (h1,t2)
      where (h1,t1) = S.breakOn pat1 body0
            len1 = B.length pat1
            t2 = B.drop len1 t1
            !pat1 = checkPattern pat0
{- ^  >>> "a\nbc\nde" / "\n"

        ("a", "bc\\nde")    -}


-- | keep needle \@ front
class SplitFront out where
    (-/)::ByteString        
            -> ByteString   
            -> out


instance SplitFront (ByteString,ByteString) where
    (-/) body0 pat0 = S.breakOn pat1 body0
         where !pat1 = checkPattern pat0
{- ^ >>> "a\nbc\nde" -/ "\n" 

        ("a", "\\nbc\\nde")     -}


-- | keep needle \@ end
class SplitEnd out where
    (/-)::ByteString       
            -> ByteString   
            -> out


instance SplitEnd (ByteString,ByteString) where
    (/-) body0 pat0 = S.breakAfter pat1 body0
         where !pat1 = checkPattern pat0
{- ^ >>> "a\nbc\nde" /- "\n"  

    ("a\\n", "bc\\nde")         -}


instance Split [ByteString] where
    (/) body0 pat0 = S.split pat1 body0
         where !pat1 = checkPattern pat0
{- ^ >>> "a bc de" / " "      -- space may be used

    \["a", "bc", "de"]      -}


instance SplitFront [ByteString] where
    (-/) body0 pat0 = S.splitKeepFront pat1 body0
         where !pat1 = checkPattern pat0
{- ^ >>> "a\nbc\nde" -/ "\n"

    \["a", "\\nbc", "\\nde"]        -}


instance SplitEnd [ByteString] where
    (/-) body0 pat0 = S.splitKeepEnd pat1 body0
         where !pat1 = checkPattern pat0
{- ^ >>> "a\nbc\nde" /- "\n"

    \["a\\n", "bc\\n", "de"]     -}

