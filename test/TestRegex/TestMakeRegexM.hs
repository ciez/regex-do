{-# LANGUAGE NoOverloadedStrings #-}
module TestRegex.TestMakeRegexM where

import Test.Hspec
import Text.Regex.Do.Type.Reexport as R
import Text.Regex.Do.Match.Regex as T
import Debug.Trace


main::IO()
main = hspec $ do
       describe "TestRegex.TestMakeRegexM" $ do
          it "RegexResult test case" $ do
            rx1 `shouldNotSatisfy` isok1
            rx2 `shouldSatisfy` isok1
        where rx1 = T.makeRegex "[["::E R.Regex
              rx2 = T.makeRegex "."::E R.Regex

              isok1 (Left e1) = traceShow e1 False
              isok1 _ = True


instance Show (R.Regex) where
    show _ = "rx"
