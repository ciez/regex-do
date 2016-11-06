module TestRegex.StringSearch where

import Prelude hiding(break)
import Test.Hspec
import Control.Exception (evaluate)
import Regexdo.TypeDo
import Regexdo.Search as S
import qualified Data.ByteString as B
import Regexdo.Convert


main::IO()
main = hspec $ do
       describe "Habase.Regex.StringSearch.Search" $ do
          it "break :" $ do
            break (Needle ":") (Haystack "0123:oid:90") `shouldBe` ("0123", "oid:90")
          it "break" $ do
            break (Needle "\n") (Haystack "a\nbc\nde") `shouldBe` ("a", "bc\nde")
          it "break front" $ do
            breakFront (Needle "\n") (Haystack "a\nbc\nde") `shouldBe` ("a", "\nbc\nde")
          it "break end" $ do
            breakEnd (Needle "\n") (Haystack "a\nbc\nde") `shouldBe` ("a\n", "bc\nde")
          it "replace" $ do
            S.replace (Needle "\n") (Replacement ",") (Haystack "a\nbc\nde") `shouldBe` "a,bc,de"
          it "split" $ do
            split (Needle "\n") (Haystack "a\nbc\nde") `shouldBe` ["a", "bc", "de"]
          it "split_sp" $ do
            split (Needle " ") (Haystack "a bc de") `shouldBe` ["a", "bc", "de"]
          it "split end" $ do
            splitEnd (Needle "\n") (Haystack "a\nbc\nde") `shouldBe` ["a\n", "bc\n", "de"]
          it "split front" $ do
            splitFront (Needle "\n") (Haystack "a\nbc\nde") `shouldBe` ["a", "\nbc", "\nde"]
          it "split regex" $ do
            split (Needle "\\s") (Haystack "abc de fghi ") `shouldBe` ["abc de fghi "]


       describe "StringSearch.Search zerolength" $ do
          it "break" $ do
            evaluate (errFn break) `shouldThrow` anyException
          it "break front" $ do
            evaluate (errFn breakFront) `shouldThrow` anyException
          it "break end" $ do
            evaluate (errFn breakEnd) `shouldThrow` anyException
          it "replace" $ do
            evaluate (S.replace (Needle B.empty) with body) `shouldThrow` anyException
          it "split" $ do
            evaluate (errFn split) `shouldThrow` anyException
          it "split end" $ do
            evaluate (errFn splitEnd) `shouldThrow` anyException
          it "split front" $ do
            evaluate (errFn splitFront) `shouldThrow` anyException

       describe "StringSearch.Search break delim not found" $ do
            it "delim not found" $ do
                break_nf `shouldBe` ( b "a\nbc\nde", B.empty)

       where   pat = Needle "\n"
               pat_sp = Needle " "
               body = Haystack "a\nbc\nde"
               body_sp = Haystack "a bc de"
               with = Replacement ","
               break1 = break pat body
               breakFront1 = breakFront pat body
               breakEnd1 = breakEnd pat body
               replace1 = S.replace pat with body
               split1 = split pat body
               split_sp1 = split pat_sp body_sp
               splitFront1 = splitFront pat body
               splitEnd1 = splitEnd pat body
               errFn fn1 = fn1 (Needle B.empty) body
               b = toByteString
               --   break delim not found
               pat_nf = Needle ":"
               break_nf = break pat_nf body