module TestRegex.TestTrim where

import Test.Hspec
import Debug.Trace
import qualified Data.Text as T (strip, pack, unpack)
import Text.Regex.Do.Trim
import Text.Regex.Do.Convert


main::IO()
main = hspec $ do
          describe "Habase.Bin.Bistro" $ do
              it "trim" $ trace (show trimmed1) trimmed1 `shouldBe` (toByteString trimmed)
              it "trim 2" $ trim sTrim `shouldBe` "aiy  \n  pdsodfg987"
    where sTrim = " \t aiy  \n  pdsodfg987  \t\t   "::String
          trimmed1 = trim $ toByteString sTrim
          trimmed = (T.unpack . T.strip . T.pack) sTrim