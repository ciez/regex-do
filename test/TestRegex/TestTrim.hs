module TestRegex.TestTrim where

import Test.Hspec
import Debug.Trace
import qualified Data.Text as T (strip, pack, unpack)
import Regexdo.Trim
import Regexdo.Convert


main::IO()
main = hspec $ do
          describe "Habase.Bin.Bistro" $ do
              it "trim" $ trace (show trimmed1) trimmed1 `shouldBe` (toByteString trimmed)
              it "trim 2" $
                trim sTrim `shouldBe` "aiy  \n  pdsodfg987"
    where sTrim = "    aiy  \n  pdsodfg987   "
          trimmed1 = trim $ toByteString sTrim
          trimmed = (T.unpack . T.strip . T.pack) sTrim