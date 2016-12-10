{-# LANGUAGE BangPatterns, NoOverloadedStrings #-}
module TestRegex.TestReplace where

import Test.Hspec
import Text.Regex.Do.Type.Do_

import Text.Regex.Do.Replace.Latin as R
import qualified Text.Regex.Do.Replace.Utf8 as U
import Text.Regex.Do.Replace.Open hiding (replace)
import Text.Regex.Do.Type.Convert
import Debug.Trace
import Text.Regex.Do.Type.MatchHint
import Data.ByteString


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
            replace (Once "\\w=(\\d{1,3})") replacer "a=101 b=3 12"
                `shouldBe` (Right "a=[сто один] b=3 12"::E String)
          it "replaceGroup 2" $ do
            replace (All "\\w=(\\d{1,3})") replacer "a=101 b=3 12"
                `shouldBe` (Right "a=[сто один] b=[three] 12"::E String)
          it "replace 3" $ do
            U.replace (Once "менее") ("более") ("менее менее")
                `shouldBe` (Right "более менее"::E String)
          it "replace 4" $ do
            U.replace (All "менее") ("боле") ("менее менее")
                `shouldBe` (Right "боле боле"::E String)
          it "replace 5" $ do
            U.replace (Once "^a\\s") ("A") "a bc хол.гор."
                `shouldBe` (Right "Abc хол.гор."::E String)


onceUtf8::IO()
onceUtf8 = hspec $ do
       describe "Pcre.Replace Once Utf8" $ do
          it "^a\\s" $ do
            replace (Once ("^a\\s")) ("A") ("a bc хол.гор.") `shouldBe` (Right "Abc хол.гор."::E String)
          it "^b\\s" $ do
            replace (Once ("^b\\s")) ("A") ("a bc хол.гор.") `shouldBe` (Right "a bc хол.гор."::E String)


latinOnceAll::IO()
latinOnceAll =  hspec $ do
         describe "Pcre.Replace" $ do
            it "Once" $ do
               runFn1 Once `shouldBe` (Right $ toByteString "a=text1 b=11 12"::E ByteString)
            it "All" $ do
               runFn1 All `shouldBe` (Right $ toByteString "a=text1 b=text1 12"::E ByteString)
            where runFn1 hint1 =
                     let   rx1 = toByteString "(?<==)(\\d{2})"
                           body1 = toByteString haystack1
                           haystack1 = "a=10 b=11 12"
                           repl1  = toByteString "text1"
                     in replace (hint1 rx1) repl1 body1



groupReplace::IO()
groupReplace =  hspec $ do
         describe "Pcre.Replace group" $ do
            it "Once" $ do
               runFn1 Once `shouldBe` (Right "a=[сто один] b=3 12"::E String)
            it "All" $ do
               runFn1 All `shouldBe` (Right "a=[сто один] b=[three] 12"::E String)
            where runFn1 opts1 =
                     let   rx1 = "\\w=(\\d{1,3})"
                           body1 = "a=101 b=3 12"
                     in replace (opts1 rx1) replacer body1


replacer::GroupReplacer String
replacer = defaultReplacer 1 tweak1
      where tweak1 str1 = case str1 of
                              "101" -> "[сто один]"
                              "3" -> "[three]"
                              otherwise -> trace str1 "?"
