{-# LANGUAGE NoOverloadedStrings #-}
module TestRegex.Format where

import Test.Hspec
import Regexdo.Format


main::IO()
main = hspec $ do
       describe "Habase.Bin.Format" $ do
          it "list arg 0,0" $ do
            format "на первое {0}, на второе {0}" ["перловка"] `shouldBe` "на первое перловка, на второе перловка"
          it "list arg 0,1" $ do
            format "Polly {0} a {1}" ["gets","cracker"] `shouldBe` "Polly gets a cracker"
          it "map arg" $ do
            format "овчинка {a} не {b}" [("a","выделки"),("b","стоит")] `shouldBe` "овчинка выделки не стоит"
          it "pad" $ do
            pad '-' 5 "abc" `shouldBe` "--abc"
