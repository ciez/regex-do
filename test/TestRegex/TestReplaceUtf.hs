{-# LANGUAGE NoOverloadedStrings #-}
module TestRegex.TestReplaceUtf where

import Test.Hspec
import Text.Regex.Do.Type.Do_
import Data.ByteString
import Text.Regex.Do.Type.Convert
import Text.Regex.Do.Replace.Utf8 as U
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Replace.Open
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
                U.replace (All "всегда") "часто" "экзамен - всегда праздник"
                        `shouldBe` out1
            it "doc 2" $
                U.replace (All "всегда") "часто" "экзамен - всегда праздник"
                        `shouldBe` out1
            it "doc 3" $
                U.replace (All "всегда") "часто" "экзамен - всегда праздник"
                        `shouldBe` out1
            it "doc 4" $
                U.replace (Once "всегда") "часто" "экзамен - всегда праздник"
                        `shouldBe` out1
            it "doc 2-bs" $
                U.replace (All $ toByteString "всегда") (toByteString "часто") (toByteString "экзамен - всегда праздник")
                        `shouldBe` (toByteString <$> out1)
            it "doc 4-2" $
                U.replace (Once "всегда") "часто" "экзамен - всегда праздник"
                        `shouldBe` out1
        where out1 = Right "экзамен - часто праздник"::E String



groupReplace::IO()
groupReplace =  hspec $ do
       describe "TestRegex.TestReplaceUtf" $ do
            it "All" $ do
               runFn1 `shouldBe` (Right $ toByteString "100メートルー右ー折後、左"::E ByteString)
            where runFn1 =
                     let rx1 = toByteString "(?<=ル)(左)"
                         body1 = toByteString "100メートル左折後、左"
                     in U.replace (All rx1) replacer body1


replacer::GroupReplacer ByteString
replacer = defaultReplacer 1 tweak1
      where tweak1 bs1 = toByteString $
                            if bs1 == toByteString "左" then
                                  "ー右ー"
                                  else "?"


groupReplace2_::IO()
groupReplace2_ =  hspec $ do
       describe "TestRegex.TestReplaceUtf" $ do
            it "боль" $ do
               runFn1 `shouldBe` (Right $ toByteString "А - Я : 5:0"::E ByteString)
            where runFn1 =
                     let rx1 = toByteString "(\\[[^\\]]+\\])"
                         body1 = toByteString "[какая боль, ][команды] : [счёт]"
                     in U.replace (All rx1) replacer2_ body1


replacer2_::GroupReplacer (ByteString)
replacer2_ = defaultReplacer 1 tweak1
      where tweak1 s1
                | s1 == toByteString "[команды]" = toByteString "А - Я"
                | s1 == toByteString "[счёт]" = toByteString "5:0"
                | s1 == toByteString "[какая боль, ]" = empty
                | otherwise = traceShow s1 $ toByteString "?"


