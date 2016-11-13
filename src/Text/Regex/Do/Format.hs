module Text.Regex.Do.Format
    (Format(..),
    ReplaceOne(..),
    Formatable) where

import Prelude as P
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Split as S (replace)
import Text.Regex.Do.Convert
import Data.ByteString as B
import Data.Text as T


type Formatable a = (Format a [a], Format a [(a,a)])

{- | ==== implemented a:

    * 'String'
    * 'ByteString'
    * 'Text'      -}

class Format a arg where
   format::a -> arg -> a


instance ReplaceOne Int a =>
    Format a [a] where
   format = foldr_idx foldFn_idx
{- ^ === index based
     >>> format "на первое {0}, на второе {0}" ["перловка"]

     "на первое перловка, на второе перловка"

     >>> format "Polly {0} a {1}" ["got","cracker"]

     "Polly got a cracker"
-}

foldFn_idx::ReplaceOne k v =>
    v -> (k, v) -> v
foldFn_idx v0 (i0, body1) = replaceOne body1 i0 v0



instance ReplaceOne a a =>
    Format a [(a, a)] where
   format = P.foldr foldFn_map
{- ^=== key based
     key may be {any a}

     >>> format "овчинка {a} не {b}" [("a","выделки"),("b","стоит")]

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
       where pat1 = Pattern $ toByteString $ "{" ++ (show k0) ++ "}"
             repl1 = Replacement $ toByteString v0
             bs1 = S.replace pat1 repl1 $ Body $ toByteString body0


instance ReplaceOne String String where
    replaceOne body0 k0 v0 = toString bs1
       where pat1 = Pattern $ toByteString $ "{" ++ k0 ++ "}"
             repl1 = Replacement $ toByteString v0
             bs1 = S.replace pat1 repl1 $ Body $ toByteString body0


instance ReplaceOne Int ByteString where
    replaceOne body0 k0 v0 = S.replace pat1 repl1 $ Body body0
       where pat1 = Pattern $ toByteString $ "{" ++ (show k0) ++ "}"
             repl1 = Replacement v0


instance ReplaceOne ByteString ByteString where
    replaceOne body0 k0 v0 = S.replace pat1 repl1 $ Body body0
       where pat1 = Pattern $ B.concat [toByteString "{", k0, toByteString "}"]
             repl1 = Replacement v0



instance ReplaceOne Int Text where
    replaceOne body0 k0 v0 = T.replace pat1 v0 body0
       where pat1 = T.pack $ "{" ++ (show k0) ++ "}"


instance ReplaceOne Text Text where
    replaceOne body0 k0 v0 = T.replace pat1 v0 body0
       where pat1 = T.concat [T.pack "{", k0, T.pack "}"]
