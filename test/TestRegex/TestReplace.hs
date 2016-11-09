{-# LANGUAGE BangPatterns, NoOverloadedStrings #-}
module TestRegex.TestReplace where

import Test.Hspec
import Text.Regex.Do.TypeDo

import Text.Regex.Do.Pcre.Replace as R
import Text.Regex.Do.Pcre.ReplaceOpen hiding (replace)
import Text.Regex.Do.Convert
import Data.ByteString as B
import Debug.Trace


main::IO()
main = do
   onceUtf8
   latinOnceAll
   groupReplace
   doc


doc::IO()
doc = hspec $ do
       describe "Pcre.Replace doc" $ do
          it "replaceGroup 1" $ do
            replace [Once,Utf8] (Pattern "\\w=(\\d{1,3})") replacer (Body "a=101 b=3 12")
                `shouldBe` "a=[сто один] b=3 12"
          it "replaceGroup 2" $ do
            replace [All,Utf8] (Pattern "\\w=(\\d{1,3})") replacer (Body "a=101 b=3 12")
                `shouldBe` "a=[сто один] b=[three] 12"
          it "replace 3" $ do
            replace [Once,Utf8] (Pattern "менее") (Replacement  "более") (Body "менее менее")
                `shouldBe` "более менее"
          it "replace 4" $ do
            replace [All,Utf8] (Pattern "менее") (Replacement  "боле") (Body "менее менее")
                `shouldBe` "боле боле"
          it "replace 5" $ do
            replace [Once,Utf8] (Pattern "^a\\s") (Replacement "A") (Body "a bc хол.гор.")
                `shouldBe` "Abc хол.гор."


onceUtf8::IO()
onceUtf8 = hspec $ do
       describe "Pcre.Replace Once Utf8" $ do
          it "^a\\s" $ do
            replace [Once,Utf8] (Pattern "^a\\s") (Replacement "A") (Body "a bc хол.гор.") `shouldBe` "Abc хол.гор."
          it "^b\\s" $ do
            replace [Once,Utf8] (Pattern "^b\\s") (Replacement "A") (Body "a bc хол.гор.") `shouldBe` "a bc хол.гор."


latinOnceAll::IO()
latinOnceAll =  hspec $ do
         describe "Pcre.Replace" $ do
            it "Once" $ do
               runFn [Once] `shouldBe` toByteString "a=text1 b=11 12"
            it "All" $ do
               runFn [All] `shouldBe` toByteString "a=text1 b=text1 12"
            where runFn opts =
                     let   rx1 = pattern "(?<==)(\\d{2})"
                           body1 = Body $ toByteString haystack1
                           haystack1 = "a=10 b=11 12"
                           repl1  = replacement "text1"
                     in replace opts rx1 repl1 body1


pattern::String -> Pattern ByteString
pattern = Pattern . toByteString


replacement::String -> Replacement ByteString
replacement = Replacement . toByteString


groupReplace::IO()
groupReplace =  hspec $ do
         describe "Pcre.Replace group" $ do
            it "Once" $ do
               runFn1 [Once,Utf8] `shouldBe` "a=[сто один] b=3 12"
            it "All" $ do
               runFn1 [All,Utf8] `shouldBe` "a=[сто один] b=[three] 12"
            where runFn1 opts1 =
                     let   rx1 = Pattern "\\w=(\\d{1,3})"
                           body1 = Body "a=101 b=3 12"
                     in replace opts1 rx1 replacer body1


replacer::GroupReplacer String
replacer = defaultReplacer 1 tweak1
      where tweak1 str1 = case str1 of
                              "101" -> "[сто один]"
                              "3" -> "[three]"
                              otherwise -> trace str1 "?"
