{-# LANGUAGE OverloadedStrings #-}
module TestRegex.TestReplaceOpen where

import Test.Hspec
import Text.Regex.Do.Pcre.ReplaceOpen as O
import Text.Regex.Do.TypeDo
import Data.Text


main::IO()
main = hspec $ do
       describe "TestRegex.TestReplaceOpen" $ do
          it "case 1" $
            O.replace (Just $ toArray [(4,3)]) (Replacement "4567") (Body "abc 123 def"::Body Text) `shouldBe` ("abc 4567 def"::Text)