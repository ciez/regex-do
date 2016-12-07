{- | see also "Text.Regex.Do.Pcre.Replace.Latin"

    __'toByteString'__ converts String to 'ByteString'     -}

module Text.Regex.Do.Replace.Utf8
    (Replace(..))  where

import Prelude as P
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Match.Matchf
import qualified Text.Regex.Do.Replace.Open as O
import Text.Regex.Do.Match.Regex as T
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Type.Convert
import Data.ByteString
import Text.Regex.Do.Type.Extract
import Text.Regex.Do.Type.Reexport as R
import Text.Regex.Base.RegexLike as R
import Text.Regex.Do.Match.Option


{- | see "Text.Regex.Do.Pcre.Replace.Latin" for implemented types

    'GroupReplacer' is implemented only for 'ByteString'    -}

class Replace hint pattern repl body out where
    replace::(Extract' body, RegexLike R.Regex body) =>
        hint pattern -> repl -> body -> out


instance Replace Once R.Regex String String String where
    replace (Once p0) r0 b0 =    
        let b1 = toByteString b0
            r1 = toByteString r0 
            Right ma1 = marray_ (Right p0) (tagOnce b1)  
        in toString $ O.replace ma1 r1 b1
-- ^ always succeeds

instance Replace All R.Regex String String String where
    replace (All p0) r0 b0 =
        let b1 = toByteString b0
            r1 = toByteString r0
            Right ma1 = marray_ (Right p0) (tagAll b1)
        in toString $ O.replace ma1 r1 b1
-- ^ always succeeds


instance O.Replace Maybe repl ByteString => 
    Replace Once R.Regex repl ByteString ByteString where
    replace (Once p0) r0 b0 =
        let Right ma1 = marray_ (Right p0) (tagOnce b0)
        in O.replace ma1 r0 b0
{- ^ succeeds unless 'GroupReplacer' fails due to mismatched pattern etc 

repl: 'ByteString' | 'GroupReplacer' 'ByteString'    
-}


instance O.Replace [] repl ByteString => 
    Replace All R.Regex repl ByteString ByteString where
    replace (All p0) r0 b0 =
        let Right ma1 = marray_ (Right p0) (tagAll b0)
        in O.replace ma1 r0 b0
{- ^ succeeds unless 'GroupReplacer' fails due to mismatched pattern etc 

repl: 'ByteString' | 'GroupReplacer' 'ByteString'    
-}



instance Replace Once String String String (E String) where
    replace (Once p0) = replace_str p0 tagOnce


instance Replace All String String String (E String) where
    replace (All p0) = replace_str p0 tagAll 


instance Replace Once ByteString ByteString ByteString (E ByteString) where
    replace (Once p0) = replace_bs p0 tagOnce


instance Replace All ByteString ByteString ByteString (E ByteString)where
    replace (All p0) = replace_bs p0 tagAll


instance Replace Once ByteString (GroupReplacer ByteString) ByteString (E ByteString) where
    replace (Once p0) = replace_bs p0 tagOnce

instance Replace All ByteString (GroupReplacer ByteString) ByteString (E ByteString) where
    replace (All p0) = replace_bs p0 tagAll
{- ^ 
@
replacer::GroupReplacer ByteString
replacer = defaultReplacer 1 tweak1
      where tweak1 bs1 = toByteString' $
                        if bs1 == toByteString "左" then
                              "ー右ー"
                               else "?"


    runFn1 \`shouldBe\` toByteString "100メートルー右ー折後、左"
        where runFn1 = let rx1 = toByteString "(?<=ル)(左)"
                           body1 = toByteString "100メートル左折後、左"
                       in replace (All rx1) replacer body1    
@    -}

replace_str p0 tag0 r0 b0 =
   let b1 = toByteString b0
       r1 = toByteString r0
       p1 = T.makeRegexOpt p0 [Utf8] []  
   in marray_ p1 (tag0 b0) >>= \ma1 ->
    Right $ toString $ O.replace ma1 r1 b1


replace_bs p0 tag0 r0 b0 =
        marray_ (T.makeRegexOpt p0 [Utf8] []) (tag0 b0) >>= \ma1 ->
    Right $ O.replace ma1 r0 b0

