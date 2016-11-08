module TestRegex.TestSplit where

import Prelude hiding(break)
import Test.Hspec
import Control.Exception (evaluate)
import Text.Regex.Do.TypeDo
import Text.Regex.Do.Split as S
import qualified Data.ByteString as B
import Text.Regex.Do.Convert


main::IO()
main = hspec $ do
       describe "Habase.Regex.StringSearch.Search" $ do
          it "break :" $ do
            break (Pattern ":") (Body "0123:oid:90") `shouldBe` ("0123", "oid:90")
          it "break" $ do
            break (Pattern "\n") (Body "a\nbc\nde") `shouldBe` ("a", "bc\nde")
          it "break front" $ do
            breakFront (Pattern "\n") (Body "a\nbc\nde") `shouldBe` ("a", "\nbc\nde")
          it "break end" $ do
            breakEnd (Pattern "\n") (Body "a\nbc\nde") `shouldBe` ("a\n", "bc\nde")
          it "replace" $ do
            S.replace (Pattern "\n") (Replacement ",") (Body "a\nbc\nde") `shouldBe` "a,bc,de"
          it "split" $ do
            split (Pattern "\n") (Body "a\nbc\nde") `shouldBe` ["a", "bc", "de"]
          it "split_sp" $ do
            split (Pattern " ") (Body "a bc de") `shouldBe` ["a", "bc", "de"]
          it "split end" $ do
            splitEnd (Pattern "\n") (Body "a\nbc\nde") `shouldBe` ["a\n", "bc\n", "de"]
          it "split front" $ do
            splitFront (Pattern "\n") (Body "a\nbc\nde") `shouldBe` ["a", "\nbc", "\nde"]
          it "split regex" $ do
            split (Pattern "\\s") (Body "abc de fghi ") `shouldBe` ["abc de fghi "]


       describe "StringSearch.Search zerolength" $ do
          it "break" $ do
            evaluate (errFn break) `shouldThrow` anyException
          it "break front" $ do
            evaluate (errFn breakFront) `shouldThrow` anyException
          it "break end" $ do
            evaluate (errFn breakEnd) `shouldThrow` anyException
          it "replace" $ do
            evaluate (S.replace (Pattern B.empty) with body) `shouldThrow` anyException
          it "split" $ do
            evaluate (errFn split) `shouldThrow` anyException
          it "split end" $ do
            evaluate (errFn splitEnd) `shouldThrow` anyException
          it "split front" $ do
            evaluate (errFn splitFront) `shouldThrow` anyException

       describe "StringSearch.Search break delim not found" $ do
            it "delim not found" $ do
                break_nf `shouldBe` ( b "a\nbc\nde", B.empty)

       where   pat = Pattern "\n"
               pat_sp = Pattern " "
               body = Body "a\nbc\nde"
               body_sp = Body "a bc de"
               with = Replacement ","
               break1 = break pat body
               breakFront1 = breakFront pat body
               breakEnd1 = breakEnd pat body
               replace1 = S.replace pat with body
               split1 = split pat body
               split_sp1 = split pat_sp body_sp
               splitFront1 = splitFront pat body
               splitEnd1 = splitEnd pat body
               errFn fn1 = fn1 (Pattern B.empty) body
               b = toByteString
               --   break delim not found
               pat_nf = Pattern ":"
               break_nf = break pat_nf body