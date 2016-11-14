module TestRegex.TestPcre where

import Test.Hspec
import Text.Regex.Do.Type.Do as M
import Text.Regex.Do.Pcre.Ascii.Match as M
import Text.Regex.Do.Pcre.Utf8.Match as U
import Text.Regex.Do.Pcre.Ascii.MatchHint as S
import Text.Regex.Do.Pcre.Utf8.MatchHint as Su
import Text.Regex.Do.Convert
import Data.ByteString
import Text.Regex.Do.Type.MatchHint as S
import Text.Regex.Do.Type.Regex


main::IO()
main = hspec $ describe " matchTest " $ do
     it " ?String " $ (M.match (Pattern n) h::[String]) `shouldBe` ["d1"]
     it " [String] " $ (M.match (Pattern n) h::[[String]]) `shouldBe` [["d1"],["d1"]]
     it " ?ByteString " $ (M.match (Pattern $ b n) (b <$> h)::[ByteString]) `shouldBe` [b "d1"]
     it " [ByteString] " $ (M.match (Pattern $ b n) (b <$> h)::[[ByteString]]) `shouldBe` [[b "d1"],[b "d1"]]
     it " ша " $ S.match (Test $ Pattern ("^ша"::String)) (Body "шапка") `shouldBe` True
     it " cd " $ S.match (Test $ Pattern ("^cd"::String)) (Body "abcde") `shouldBe` False
     it " cd " $ S.match (PosLen' $ Pattern ("^cd"::String)) (Body "abcde") `shouldBe` []
     it " ab " $ S.match (Test $ Pattern ("^ab"::String)) (Body "abc") `shouldBe` True
     it "doc 1" $ Test ("в"::ByteString) S.=~ "тихо в лесу" `shouldBe` True
     it "doc 1.2" $ Test ("chilly"::ByteString) S.=~ "it's chilly inside, chilly outside" `shouldBe` True
     it "doc 1.3" $ Test (toByteString "в") Su.=~ toByteString "тихо в лесу" `shouldBe` True
     it "doc 2" $ S.Once ("^all"::String) S.=~ "all the time" `shouldBe` ["all"]
     it "doc 3" $ S.Once ("^all"::ByteString) S.=~ "all the time" `shouldBe` ["all"]
     it "doc 4" $ S.All ("chilly"::ByteString) S.=~ "it's chilly inside, chilly outside" `shouldBe` ([["chilly"],["chilly"]])
     it "doc 5" $ PosLen' ("и"::String) S.=~ "бывает и хуже" `shouldBe` [(13,2)]
     it "doc 6" $ PosLen' ("à"::String) S.=~ "tourner à gauche" `shouldBe` [(8,2)]
     it "doc 7" $ do
        let rx1 = makeRegexOpt' (Pattern $ toByteString'"左") [] []
            rx2 = Utf8_ <$> rx1
            m1 = U.match rx2 (Body $ toByteString' "100メートル左折後、左")::[ByteString]
        m1 `shouldBe` [toByteString "左"]
     it "doc 8" $ do
        let t1 = toByteString "в" U.=~ ("тихо в лесу"::String)::Bool
        t1 `shouldBe` True
     it "doc 9" $ do
        let m1 = ("well"::String) U.=~ ("all is well that ends well"::String)::[[String]]
        m1 `shouldBe` ([["well"],["well"]]::[[String]])

n = "d1"
h = Body "abcd1efg d1hij"
b = toByteString
