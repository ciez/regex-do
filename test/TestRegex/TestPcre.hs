module TestRegex.TestPcre where

import Test.Hspec
import Text.Regex.Do.Type.Do as M
import Text.Regex.Do.Pcre.Match as M
import Text.Regex.Do.Convert
import Data.ByteString
import Text.Regex.Do.Pcre.MatchSame as M

main::IO()
main = hspec $ describe " matchTest " $ do
     it " ?String " $ (M.match (Pattern n) h::[String]) `shouldBe` ["d1"]
     it " [String] " $ (M.match (Pattern n) h::[[String]]) `shouldBe` [["d1"],["d1"]]
     it " ?ByteString " $ (M.match (Pattern $ b n) (b <$> h)::[ByteString]) `shouldBe` [b "d1"]
     it " [ByteString] " $ (M.match (Pattern $ b n) (b <$> h)::[[ByteString]]) `shouldBe` [[b "d1"],[b "d1"]]
     it " ша " $ M.match' (Pattern ("^ша"::String)) (Body "шапка") `shouldBe` True
     it " cd " $ M.match' (Pattern ("^cd"::String)) (Body "abcde") `shouldBe` False
     it " cd " $ M.match' (Pattern ("^cd"::String)) (Body "abcde") `shouldBe` ([]::[PosLen])
     it " ab " $ M.match' (Pattern ("^ab"::String)) (Body "abc") `shouldBe` True
     it "doc 1" $ ("в"::ByteString) -~ "тихо в лесу" `shouldBe` True
     it "doc 2" $ ("^all"::String) -~ "all the time" `shouldBe` ["all"::String]
     it "doc 3" $ ("^all"::ByteString) -~ "all the time" `shouldBe` (["all"]::[ByteString])
     it "doc 4" $ ("well"::ByteString) -~ "all is well that ends well" `shouldBe` ([["well"],["well"]]::[[ByteString]])
     it "doc 5" $ ("и"::String) -~ "бывает и хуже" `shouldBe` ([(13,2)]::[PosLen])

n = "d1"
h = Body "abcd1efg d1hij"
b = toByteString
