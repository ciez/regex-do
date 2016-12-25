{-# LANGUAGE OverloadedStrings #-}
module TestRegex.TestReplaceOpen where

import Test.Hspec
import Text.Regex.Do.Replace.Open as O
import Text.Regex.Do.Type.Do_
import Data.Text
import Text.Regex.Do.Type.Convert()
import Debug.Trace


main::IO()
main = hspec $ do
       describe "TestRegex.TestReplaceOpen" $ do
          it "case 1" $
            O.replace (Just [(4,3)::PosLen]) ("4567"::Text) ("abc 123 def"::Text) `shouldBe` ("abc 4567 def"::Text)
          it "case 2" $
            O.replace (Just [(4,3)::PosLen]) replacer ("abc 123 def"::Text) `shouldBe` ("abc [1-2-3] def"::Text)
          it "case 3" $
            O.replace ([[(4,3)::PosLen]]) replacer ("abc 123 def"::Text) `shouldBe` ("abc [1-2-3] def"::Text)
          it "case 4" $
            O.replace ([[]::[PosLen]]) replacer ("abc 123 def"::Text) `shouldBe` ("abc 123 def"::Text)
          it "case 5" $
            O.replace ([[(4,3)],[(8,3)],[(16,3)]]::[[PosLen]]) replacer ("abc 456 456 def 456"::Text) `shouldBe` ("abc 4 4 def 4"::Text)


replacer::GroupReplacer Text
replacer = defaultReplacer 1 tweak1
          where tweak1 str1 = case str1 of
                                "123" -> "[1-2-3]"
                                "456" -> "4"
                                otherwise -> traceShow str1 "?"