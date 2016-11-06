{-# LANGUAGE FlexibleContexts, NoOverloadedStrings #-}
module TestRegex.Pcre where

import Test.Hspec
import Regexdo.TypeDo
import Regexdo.Pcre.Match as M
import qualified Regexdo.Pcre.Result as R
import Regexdo.Convert



main::IO()
main = do
   test " ?String " Needle id M.match
   test " [String] " Needle id M.matchAll
   test " ?Bs " (Needle . b) (b <$>) M.match
   test " [Bs] " (Needle . b) (b <$>) M.matchAll
   hspec $ do
    describe " matchTest " $ do
     it " matchTest " $ do
        M.matchTest (Needle "^ша") (Haystack "шапка") `shouldBe` True
        M.matchTest (Needle "^cd") (Haystack "abcde") `shouldBe` False
        M.matchTest (Needle "^ab") (Haystack "abc") `shouldBe` True

n = "d1"
h = Haystack "abcd1efg d1hij"
b = toByteString


test testCase patternCtor bodyCtor matchFn =   hspec $ do
   describe testCase $ do
         it " val " $ do
            R.value h res `shouldSatisfy` check
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
