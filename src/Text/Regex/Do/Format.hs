module Text.Regex.Do.Format
    (Format(..)) where

import Prelude as P
import Text.Regex.Do.TypeDo
import Text.Regex.Do.Split as S (replace)
import Text.Regex.Do.Convert


class Format a where
   format::String -> a -> String


instance Format [String] where
   format = foldr_idx foldFn_idx
-- ^
-- === index based
-- >>> format "на первое {0}, на второе {0}" ["перловка"]
--
-- "на первое перловка, на второе перловка"
--
-- >>> format "Polly {0} a {1}" ["got","cracker"]
--
-- "Polly got a cracker"
--

foldFn_idx::String -> (Int, String) -> String
foldFn_idx v (i,body1) = replaceOne body1 (show i) v


instance Format [(String,String)] where
   format = P.foldr foldFn_map
-- ^
-- === key based
-- key may be {any string}
--
-- >>> format "овчинка {a} не {b}" [("a","выделки"),("b","стоит")]
--
-- "овчинка выделки не стоит"
--

foldFn_map:: (String, String) -> String -> String
foldFn_map (k,v) body1 = replaceOne body1 k v

replaceOne::String -> String -> String -> String
replaceOne body k v = toString bs1
   where pat1 = Pattern $ toByteString $ "{" ++ k ++ "}"
         repl1 = Replacement $ toByteString v
         bs1 = S.replace pat1 repl1 $ Body $ toByteString body



--  fold with index
type CustomerFn a b = (a -> (Int,b) -> b)

foldr_idx :: CustomerFn a b -> b -> [a] -> b
foldr_idx fn init1 list = b1
   where i0 = P.length list - 1
         (-1,b1) = P.foldr (foldFn fn) (i0,init1) list

foldFn :: CustomerFn a b -> a -> (Int,b) -> (Int,b)
foldFn fn val t@(i,_)= (i-1,b1)
   where b1 = fn val t
