{-# LANGUAGE FlexibleContexts, NoOverloadedStrings #-}
module TestRegex.TestPcre where

import Test.Hspec
import Text.Regex.Do.TypeDo as M
import Text.Regex.Do.Pcre.Match as M
import qualified Text.Regex.Do.Pcre.Result as R
import Text.Regex.Do.Convert



main::IO()
main = do
   test " ?String " Pattern id M.matchOnce'
   test " [String] " Pattern id M.matchAll'
   test " ?Bs " (Pattern . b) (b <$>) M.matchOnce'
   test " [Bs] " (Pattern . b) (b <$>) M.matchAll'
   hspec $ do
    describe " matchTest " $ do
     it " matchTest " $ do
        M.matchTest (Pattern "^ша") (Body "шапка") `shouldBe` True
        M.matchTest (Pattern "^cd") (Body "abcde") `shouldBe` False
        M.matchTest (Pattern "^ab") (Body "abc") `shouldBe` True

n = "d1"
h = Body "abcd1efg d1hij"
b = toByteString


test testCase patternCtor bodyCtor matchFn =   hspec $ do
   describe testCase $ do
         it " val " $ do
            R.allMatches h res `shouldSatisfy` check
         it " tup " $
            R.poslen res  `shouldSatisfy` check
         it " pl " $
            R.poslen res  `shouldSatisfy` check
   where   res = matchFn pat $ bodyCtor h
           pat = patternCtor n



class Functor f =>  Pred f x where
   check::f x -> Bool

instance Pred Maybe [String] where
   check Nothing = False
   check (Just _) = True

instance Pred [] [String] where
   check [] = False
   check (h:t) = True

instance Pred Maybe [PosLen] where
   check Nothing = False
   check (Just _) = True

instance Pred [] [PosLen] where
   check [] = False
   check (h:t) = True
