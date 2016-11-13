{-# LANGUAGE NoOverloadedStrings #-}
module TestRegex.TestReplaceUtf where

import Test.Hspec
import Text.Regex.Do.Type.Do
import Data.ByteString
import Text.Regex.Do.Convert
import Text.Regex.Do.Pcre.Utf8.Replace as U
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.ReplaceOpen
import Debug.Trace


main::IO()
main = do
     groupReplace
     groupReplace2_
     doc


doc::IO()
doc = hspec $ do
       describe "TestRegex.TestReplaceUtf doc" $ do
            it "doc 1" $
                U.replace (All "праздник") (Replacement "радость") "экзамен - всегда праздник"
                        `shouldBe` out1
            it "doc 2" $
                U.replace "праздник" (All (Replacement "радость")) "экзамен - всегда праздник"
                        `shouldBe` out1
            it "doc 3" $
                U.replace "праздник" (Replacement "радость") (All "экзамен - всегда праздник")
                        `shouldBe` out1
            it "doc 4" $
                U.replace "праздник" (Replacement "радость") (Once "экзамен - всегда праздник")
                        `shouldBe` out1
            it "doc 2-bs" $
                U.replace (toByteString "праздник") (All (Replacement $ toByteString "радость")) (toByteString "экзамен - всегда праздник")
                        `shouldBe` (toByteString out1)
            it "doc 4-2" $
                U.replace (Once $ Pattern $ Utf8_ "праздник") (Replacement $ Utf8_ "радость") (Body $ Utf8_ "экзамен - всегда праздник")
                        `shouldBe` out1
        where out1 = "экзамен - всегда радость"



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


groupReplace2_::IO()
groupReplace2_ =  hspec $ do
       describe "TestRegex.TestReplaceUtf" $ do
            it "боль" $ do
               runFn1 `shouldBe` (toByteString "А - Я : 5:0")
            where runFn1 =
                     let rx1 = toByteString "(\\[[^\\]]+\\])"
                         body1 = toByteString "[какая боль, ][команды] : [счёт]"
                     in U.replace rx1 (All replacer2_) body1


replacer2_::GroupReplacer (ByteString)
replacer2_ = defaultReplacer 1 tweak1
      where tweak1 s1
                | s1 == toByteString "[команды]" = toByteString "А - Я"
                | s1 == toByteString "[счёт]" = toByteString "5:0"
                | s1 == toByteString "[какая боль, ]" = empty
                | otherwise = traceShow s1 $ toByteString "?"


