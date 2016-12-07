module TestRegex.TestSplit where

import Prelude hiding((/))
import Test.Hspec
import Control.Exception (evaluate)
import Text.Regex.Do.Split as S
import Text.Regex.Do.Replace.Fast as S
import qualified Data.ByteString as B
import Text.Regex.Do.Type.Convert



main::IO()
main = hspec $ do
       describe "Habase.Regex.StringSearch.Search" $ do
          it "break :" $ do
            "0123:oid:90"  S./ ":"  `shouldBe` (("0123", "oid:90")::S.T)
          it "break" $ do
            "a\nbc\nde" / "\n" `shouldBe` (("a", "bc\nde")::S.T)
          it "break front" $ do
            "a\nbc\nde" -/ "\n" `shouldBe` (("a", "\nbc\nde")::S.T)
          it "break end" $ do
            "a\nbc\nde" /- "\n"  `shouldBe` (("a\n", "bc\nde")::S.T)
          it "replace" $ do
            S.replace "\n" "," "a\nbc\nde" `shouldBe` "a,bc,de"
          it "split" $ do
            "a\nbc\nde" / "\n" `shouldBe` (["a", "bc", "de"]::S.L)
          it "split_sp" $ do
            "a bc de" / " " `shouldBe` (["a", "bc", "de"]::S.L)
          it "split end" $ do
            "a\nbc\nde" /- "\n"  `shouldBe` (["a\n", "bc\n", "de"]::S.L)
          it "split front" $ do
            "a\nbc\nde" -/ "\n"  `shouldBe` (["a", "\nbc", "\nde"]::S.L)
          it "split regex" $ do
            "abc de fghi " / "\\s"  `shouldBe` (["abc de fghi "]::S.L)


       describe "StringSearch.Search zerolength" $ do
          it "break" $ do
            evaluate (body / z1::S.T) `shouldThrow` anyException
          it "break front" $ do
            evaluate (body -/ z1::S.T) `shouldThrow` anyException
          it "break end" $ do
            evaluate (body /- z1::S.T) `shouldThrow` anyException
          it "replace" $ do
            evaluate (S.replace B.empty with body) `shouldThrow` anyException
          it "split" $ do
            evaluate (body / z1::S.T) `shouldThrow` anyException
          it "split end" $ do
            evaluate (body -/ z1::S.T) `shouldThrow` anyException
          it "split front" $ do
            evaluate (body -/ z1::S.T) `shouldThrow` anyException

       describe "StringSearch.Search break delim not found" $ do
            it "delim not found" $ do
                break_nf `shouldBe` ( b "a\nbc\nde", B.empty)

       where   pat = "\n"
               pat_sp = " "
               body = "a\nbc\nde"
               body_sp = "a bc de"
               with = ","
               break1 = body / pat::S.T 
               breakFront1 = body -/ pat::S.T
               breakEnd1 = body /- pat::S.T
               replace1 = S.replace pat with $ body
               split1 = body / pat::S.L 
               split_sp1 = body_sp / pat_sp::S.L 
               splitFront1 = body -/ pat::S.L  
               splitEnd1 = body /- pat::S.L  
               z1 = B.empty
               b = toByteString
               --   break delim not found
               break_nf = body / ":"  