{-# LANGUAGE OverloadedStrings #-}
module TestRegex.TestReplaceOpen where

import Test.Hspec
import Text.Regex.Do.Pcre.ReplaceOpen as O
import Text.Regex.Do.Type.Do
import Data.Text
import Text.Regex.Do.Convert()
import Debug.Trace


main::IO()
main = hspec $ do
       describe "TestRegex.TestReplaceOpen" $ do
          it "case 1" $
            O.replace (Just [(4,3)::PosLen]) (Replacement "4567") (Body "abc 123 def"::Body Text) `shouldBe` ("abc 4567 def"::Text)
          it "case 2" $
            O.replace (Just ([(4,3),(8,2)]::[PosLen])) replacer (Body "abc 123 def"::Body Text) `shouldBe` ("abc [1-2-3] def"::Text)


replacer::GroupReplacer Text
replacer = defaultReplacer 1 tweak1
          where tweak1 str1 = case str1 of
                                "123" -> "[1-2-3]"
                                otherwise -> traceShow str1 "?"