{-# LANGUAGE BangPatterns, NoOverloadedStrings #-}
module TestRegex.Replace where

import Test.Hspec
import Data.Array((!))
import Regexdo.TypeDo

import Regexdo.Pcre.Replace
import Regexdo.Convert
import Data.ByteString as B
import Text.Regex.Base.RegexLike


main::IO()
main = do
   onceUtf8
   latinOnceAll
   groupReplace
   doc


doc::IO()
doc = hspec $ do
       describe "Pcre.Replace doc" $ do
          it "replace" $ do
            replace [Once,Utf8] (Needle "поп", Replacement  "крестьянин") (Haystack "у попа была собака") `shouldBe` "у крестьянина была собака"



onceUtf8::IO()
onceUtf8 = hspec $ do
       describe "Pcre.Replace Once Utf8" $ do
          it "^a\\s" $ do
            replace [Once,Utf8] (Needle "^a\\s", Replacement "A") (Haystack "a bc хол.гор.") `shouldBe` "Abc хол.гор."
          it "^b\\s" $ do
            replace [Once,Utf8] (Needle "^b\\s", Replacement "A") (Haystack "a bc хол.гор.") `shouldBe` "a bc хол.гор."


latinOnceAll::IO()
latinOnceAll =  hspec $ do
         describe "Pcre.Replace" $ do
            it "Once" $ do
               runFn [Once] `shouldBe` toByteString "a=R1 b=11 12"
            it "All" $ do
               runFn [All] `shouldBe` toByteString "a=R1 b=R1 12"
            where runFn opts =
                     let   rx1 = pattern "(?<==)(\\d{2})"
                           body = Haystack $ toByteString haystack
                           haystack = "a=10 b=11 12"
                           repl1  = replacement "R1"
                     in replace opts (rx1,repl1) body


pattern::String -> Needle ByteString
pattern = Needle . toByteString


replacement::String -> Replacement ByteString
replacement = Replacement . toByteString


groupReplace::IO()
groupReplace =  hspec $ do
         describe "Pcre.Replace group" $ do
            it "Once" $ do
               runFn1 [Once] `shouldBe` "a=[A] b=3 12"
            it "All" $ do
               runFn1 [All] `shouldBe` "a=[A] b=[Be] 12"
            where runFn1 opts1 =
                     let   rx1 = Needle "(\\w)(=)(\\d{1,3})"
                           body1 = Haystack "a=101 b=3 12"
                     in replaceGroup opts1 (rx1,replacer) body1


replacer::GroupReplacer String
replacer marr1 acc1 = case val1 of
                          "101" -> fn1 "[A]"
                          "3" -> fn1 "[Be]"
   where ol1 = marr1 ! 3 :: (MatchOffset, MatchLength)
         val1 = extract ol1 acc1
--         !val2 = trace (show val1) val1
         fn1 str1 = replaceMatch ol1 (str1,acc1)
