module TestRegex.TestReplaceUtf where

import Test.Hspec
import Text.Regex.Do.Type.Do
import Data.ByteString
import Text.Regex.Do.Convert
import Text.Regex.Do.Pcre.Utf8.Replace as U
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.ReplaceOpen


main::IO()
main = groupReplace

groupReplace::IO()
groupReplace =  hspec $ do
       describe "TestRegex.TestReplaceUtf" $ do
            it "All" $ do
               runFn1 `shouldBe` toByteString "100メートルー右ー折後、左"
            where runFn1 =
                     let rx1 = Pattern $ toByteString' "(?<=ル)(左)"
                         body1 = Body $ toByteString' "100メートル左折後、左"
                     in U.replace (All rx1) replacer body1


replacer::GroupReplacer (Utf8_ ByteString)
replacer = defaultReplacer 1 tweak1
      where tweak1 bs1 = toByteString' $
                            if bs1 == toByteString' "左" then
                                  "ー右ー"
                                  else "?"
