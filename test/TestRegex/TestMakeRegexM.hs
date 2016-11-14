{-# LANGUAGE NoOverloadedStrings #-}
module TestRegex.TestMakeRegexM where

import Test.Hspec
import Text.Regex.Do.Type.Reexport as R
import Text.Regex.Do.Type.Regex as T
import Text.Regex.Do.Type.Do as T
import Debug.Trace


main::IO()
main = hspec $ do
       describe "TestRegex.TestMakeRegexM" $ do
          it "RegexResult test case" $ do
            rx1 `shouldNotSatisfy` isok1
            rx2 `shouldSatisfy` isok1
        where rx1 = T.makeRegexM $ Pattern "[["::RegexResult R.Regex
              rx2 = T.makeRegexM $ Pattern "."::RegexResult R.Regex

              isok1 (RegexResult (Left e1)) = traceShow e1 False
              isok1 _ = True


instance Show (RegexResult R.Regex) where
    show _ = "rx"
