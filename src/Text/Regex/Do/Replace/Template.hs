{- | formerly /Format/

    substitutes placeholder values with values from args
    
    in lieu of /format/ function there are 2 operators: '<' and '>': no need to remember arg order.  

    ==== placeholder syntax:

       * index based

            { Int }

       * key based

            { 'String' | 'ByteString' | 'Text' }

    other placeholders may be implemented with

    "Text.Regex.Do.Replace.Latin" or "Text.Regex.Do.Replace.Utf8"       
    
    to avoid clash with 'Prelude':
    
    @import Prelude hiding((\<),(\>))@       

    or qualify '<' with alias e.g. (assuming this module is imported with F alias):
       
    @F.<@       -}

module Text.Regex.Do.Replace.Template
    (Template(..),
    ReplaceOne(),
    Formatable) where

import Prelude as P hiding ((<),(>))
import Text.Regex.Do.Replace.Fast as S (replace)
import Text.Regex.Do.Type.Convert
import Data.ByteString as B
import Data.Text as T


type Formatable a = (Template a [a], Template a [(a,a)])

{- | ==== implemented a:

    * 'String'
    * 'ByteString'
    * 'Text'      
    
    a: template e.g. "today is {0}"
    
    repl: replacement: [a] or [(a,a)]
    -}
class Template a repl where
   (<)::a -> repl -> a
   (>)::repl -> a -> a
   (>) repl0 template0 = template0 < repl0
   

instance ReplaceOne Int a =>
    Template a [a] where
   (<) t0 a0 = foldr_idx foldFn_idx t0 a0
{- ^ === index based
    >>> ["цветы", "мороженное"] > "даме {0}, детям {1}" 
    
    "даме цветы, детям мороженное"

     >>> "Polly {0} a {1}" < ["wants","cracker"]

     "Polly wants a cracker"
     
     >>> ["перловка"] > "на первое {0}, на второе {0}"
    
     "на первое перловка, на второе перловка"
-}

foldFn_idx::ReplaceOne k v =>
    v -> (k, v) -> v
foldFn_idx v0 (i0, body1) = replaceOne body1 i0 v0



instance ReplaceOne a a =>
    Template a [(a, a)] where
   (<) = P.foldr foldFn_map
{- ^=== key based
     key may be {any a}

     >>> "овчинка {a} не {b}" < [("a","выделки"),("b","стоит")]

     "овчинка выделки не стоит"
-}



foldFn_map::ReplaceOne k v =>
    (k, v) -> v -> v
foldFn_map (k0, v0) body1 = replaceOne body1 k0 v0



--  fold with index
type CustomerFn a b = (a -> (Int,b) -> b)

foldr_idx::CustomerFn a b -> b -> [a] -> b
foldr_idx fn0 init1 list0 = b1
   where i0 = P.length list0 - 1
         (-1,b1) = P.foldr (foldFn fn0) (i0,init1) list0


foldFn::CustomerFn a b -> a -> (Int, b) -> (Int, b)
foldFn fn0 val0 t0@(i0, _) = (i0 - 1, b1)
   where b1 = fn0 val0 t0


class ReplaceOne idx a where
    replaceOne::a -> idx -> a -> a


instance ReplaceOne Int String where
    replaceOne body0 k0 v0 = toString bs1
       where pat1 = toByteString $ "{" ++ (show k0) ++ "}"
             repl1 = toByteString v0
             bs1 = S.replace pat1 repl1 $ toByteString body0


instance ReplaceOne String String where
    replaceOne body0 k0 v0 = toString bs1
       where pat1 = toByteString $ "{" ++ k0 ++ "}"
             repl1 = toByteString v0
             bs1 = S.replace pat1 repl1 $ toByteString body0


instance ReplaceOne Int ByteString where
    replaceOne body0 k0 v0 = S.replace pat1 repl1 body0
       where pat1 =  toByteString $ "{" ++ (show k0) ++ "}"
             repl1 = v0


instance ReplaceOne ByteString ByteString where
    replaceOne body0 k0 v0 = S.replace pat1 repl1 body0
       where pat1 = B.concat [toByteString "{", k0, toByteString "}"]
             repl1 = v0



instance ReplaceOne Int Text where
    replaceOne body0 k0 v0 = T.replace pat1 v0 body0
       where pat1 = T.pack $ "{" ++ (show k0) ++ "}"


instance ReplaceOne Text Text where
    replaceOne body0 k0 v0 = T.replace pat1 v0 body0
       where pat1 = T.concat [T.pack "{", k0, T.pack "}"]
