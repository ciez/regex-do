module TestRegex.TestFormat where

import Test.Hspec
import Text.Regex.Do.Replace.Template
import Text.Regex.Do.Pad
import Data.String
import Text.Regex.Do.Type.Convert
import Data.ByteString
import Data.Text as T
import Prelude hiding ((<),(>))


main::IO()
main = do
    oneFormat s show                        --  String
    oneFormat b $ toByteString . show       --  ByteString
    oneFormat t $ T.pack . show       --  Text


oneFormat::(Formatable a, IsString a, Eq a, Show a) =>
        (String -> a) ->
        (Int -> a) ->
            IO()
oneFormat fn0 idx0 = hspec $ do
       describe "Habase.Bin.Format" $ do
          it "list arg 0,0 repl []" $
            (fn0 "на первое {0}, на второе {0}") < (fn0 <$> []) `shouldBe` (fn0 "на первое {0}, на второе {0}")
          it "list arg 0,0" $
            (fn0 "на первое {0}, на второе {0}") < [fn0 "перловка"] `shouldBe` (fn0 "на первое перловка, на второе перловка")
          it "list arg 0,0  >" $
            [fn0 "перловка"] > (fn0 "на первое {0}, на второе {0}") `shouldBe` (fn0 "на первое перловка, на второе перловка")
          it "list arg 0,0  > 2" $
            [fn0 "цветы", fn0 "мороженое"] > (fn0 "даме {0}, детям {1}") `shouldBe` (fn0 "даме цветы, детям мороженое")
          it "list arg 0,1" $ do
            (fn0 "Polly {0} a {1}") < [fn0 "gets",fn0 "cracker"] `shouldBe` (fn0 "Polly gets a cracker")
            (fn0 "{10} {15} {21}") < (idx0 <$> [0..22]) `shouldBe` (fn0 "10 15 21")
            (fn0 "{ten} {пятнадцать} {vingt}") < [(fn0 "ten", fn0 "10"), (fn0 "пятнадцать", fn0 "15"), (fn0 "vingt", fn0 "20")] `shouldBe` (fn0 "10 15 20")
          it "map arg" $
            (fn0 "овчинка {a} не {b}") < [(fn0 "a", fn0 "выделки"),(fn0 "b", fn0 "стоит")] `shouldBe` (fn0 "овчинка выделки не стоит")

          it "pad" $ pad '-' 5 "abc" `shouldBe` "--abc"
          it "pad'" $ pad' '-' 5 "abc" `shouldBe` "abc--"
          it "pad" $ pad '-' 3 "abcde" `shouldBe` "abcde"


s::String -> String
s = id

b::String -> ByteString
b = toByteString

t::String -> Text
t = T.pack
