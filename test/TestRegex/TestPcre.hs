module TestRegex.TestPcre where

import Test.Hspec
import Text.Regex.Do.Type.Do as M
import Text.Regex.Do.Match.Latin as M
import Text.Regex.Do.Match.Utf8 as U
import Text.Regex.Do.Type.Convert
import Data.ByteString
import Text.Regex.Do.Match.Regex
import Text.Regex.Do.Match.Option


main::IO()
main = hspec $ describe " matchTest " $ do
     it " ?String " $ (n M.~? h::E [String]) `shouldBe` (Right ["d1"])
     it " [String] " $ (n M.~* h::E [[String]]) `shouldBe` (Right [["d1"],["d1"]])
     it " ?ByteString " $ (b n M.~? (b h)::E [ByteString]) `shouldBe` (Right [b "d1"])
     it " [ByteString] " $ (b n M.~* (b h)::E [[ByteString]]) `shouldBe` (Right [[b "d1"],[b "d1"]])
     it " ша " $ ("^ша"::String) M.~? ("шапка"::String) `shouldBe` (Right True::E Bool)
     it " cd " $ ("^cd"::String) M.~? ("abcde"::String) `shouldBe` (Right False::E Bool)
     it " cd " $ ("^cd"::String) M.~? ("abcde"::String) `shouldBe` (Right []::E [PosLen])
     it " ab " $ ("^ab"::String) M.~? ("abc"::String) `shouldBe` (Right True::E Bool)
     it "doc 1" $ ("в"::String) U.~? ("тихо в лесу"::String) `shouldBe` (Right True::E Bool)
     it "doc 1.2" $ ("chilly"::ByteString) U.~? ("it's chilly inside, chilly outside"::ByteString) `shouldBe` (Right True::E Bool)
     it "doc 1.3" $ (toByteString "в") U.~? toByteString "тихо в лесу" `shouldBe` (Right True::E Bool)
     it "doc 2" $ ("^all"::String) M.~? ("all the time"::String) `shouldBe` (Right ["all"]::E [String])
     it "doc 3" $ ("^all"::ByteString) M.~? ("all the time"::ByteString) `shouldBe` (Right ["all"]::E [ByteString])
     it "doc 4" $ ("chilly"::ByteString) M.~* ("it's chilly inside, chilly outside"::ByteString) `shouldBe` (Right [["chilly"],["chilly"]]::E [[ByteString]])
     it "doc 5" $ ("и"::String) U.~? ("бывает и хуже"::String) `shouldBe` (Right [(13,2)]::E [PosLen])
     it "doc 6" $ ("à"::String) U.~? ("tourner à gauche"::String) `shouldBe` (Right [(8,2)]::E [PosLen])
     it "doc 7" $ do
        let Right rx1 = makeRegexOpt (toByteString "左") [Utf8] []
            m1 = rx1 U.~? (toByteString "100メートル左折後、左")::[ByteString]
        m1 `shouldBe` [toByteString "左"]
     it "doc 8" $ do
        let Right t1 = ("в"::String) U.~? ("тихо в лесу"::String)::E Bool
        t1 `shouldBe` True
     it "doc 9" $ do
        let Right m1 = ("лес"::String) U.~* ("Залесью, залесью…"::String)::E [[String]]
        m1 `shouldBe` ([["лес"],["лес"]]::[[String]])
     it "doc 10" $ do
        let Right m1 = ("^熱"::String) U.~? ("熱い午後"::String)::E [String]
        m1 `shouldBe` ["熱"]       

n = "d1"
h = "abcd1efg d1hij"
b = toByteString
