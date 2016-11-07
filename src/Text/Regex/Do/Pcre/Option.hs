module Text.Regex.Do.Pcre.Option (
      Comp(..),
      Exec(..),
      comp,
      exec
   ) where

import Data.Bits
import qualified Text.Regex.PCRE.ByteString as B


data Comp = Blank       -- ^ 'B.compBlank'
            | Anchored  -- ^ 'B.compAnchored'
            | Caseless  -- ^ 'B.compCaseless'
            | Dotall    -- ^ 'B.compDotAll'
            | Multiline -- ^ 'B.compMultiline'
            | Utf8      -- ^ 'B.compUTF8'
            | Ungreedy  -- ^ 'B.compUngreedy'
   deriving Enum


data Exec = BlankE  -- ^ 'B.execBlank'
            | NotEmpty  -- ^ 'B.execNotEmpty'
            | Partial   -- ^ 'B.execPartial'
   deriving Enum


compOpt::Comp -> B.CompOption
compOpt o = case o of
   Blank -> B.compBlank
   Anchored -> B.compAnchored
   Caseless -> B.compCaseless
   Dotall -> B.compDotAll
   Multiline -> B.compMultiline
   Utf8 -> B.compUTF8
   Ungreedy -> B.compUngreedy


comp::[Comp] -> B.CompOption
comp [] = B.compBlank
comp l = foldl (.&.) o0 l1
   where l1 = compOpt <$> l
         o0 = compOpt $ head l


execOpt::Exec -> B.ExecOption
execOpt o =   case o of
   BlankE -> B.execBlank
   NotEmpty -> B.execNotEmpty
   Partial -> B.execPartial


exec::[Exec] -> B.ExecOption
exec [] = B.execBlank
exec l = foldl (.&.) o0 l1
   where l1 = execOpt <$> l
         o0 = execOpt $ head l