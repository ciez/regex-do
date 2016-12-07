{- |  simple (no regex) fast replace on 'B.ByteString's 

All occurrences are replaced. There is no Once option

see 'S.replace' for detail   -}
module Text.Regex.Do.Replace.Fast where

import qualified Data.ByteString.Search as S
import Data.ByteString as B 
import qualified Data.ByteString.Lazy as L
import Text.Regex.Do.Match.Matchf


{- | >>> replace "\n" "," "a\nbc\nde"

    "a,bc,de"       -}
replace::ByteString -- ^ Pattern 
        -> ByteString  -- ^ Replacement
        -> ByteString   -- ^ Body
        -> ByteString
replace pat0
  replacement0
  body0 = B.concat . L.toChunks $ l
  where l = S.replace pat1 replacement0 body0
        !pat1 = checkPattern pat0
