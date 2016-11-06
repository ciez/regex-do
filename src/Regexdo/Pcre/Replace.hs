module Regexdo.Pcre.Replace(
   ReplaceCase(..),
   Replace_cl(..),
   replaceGroup,
   replaceMatch
   )  where

import qualified Data.ByteString as B
import qualified Text.Regex.Base.RegexLike as R

import Regexdo.TypeDo
import Regexdo.TypeRegex
import Regexdo.Pcre.Match
import Regexdo.Pcre.Result
import qualified Regexdo.Pcre.Option as O
import Regexdo.Convert

data ReplaceCase = Once | All | Utf8 | Multiline deriving Eq

type Mm a = (Match_cl Regex a, Match_opt a)
type Rm a = (Replace_cl' a, Match_cl Regex a)


addOpt::Match_opt a =>
    Needle a -> [O.Comp] -> Needle Regex
addOpt pat1 opt1 = Needle rx1
    where rx1 = makeRegexOpts opt1 [] pat1



ronce::Rm a =>
    (Needle Regex, Replacement a) -> Haystack a -> a
ronce (pat1, Replacement repl1) body1@(Haystack bs_str1) =
      let pl2 = do
               let m1 = match pat1 body1
               poslen m1
      in case pl2 of
         Nothing -> bs_str1
         Just pl_arr -> firstGroup pl_arr (repl1,bs_str1)


rall::Rm a =>
    (Needle Regex, Replacement a) -> Haystack a -> a
rall (pat1, Replacement repl1) body1@(Haystack bs_str1) =
      let pl_arr_arr1 = do
               let m1 = matchAll pat1 body1
               poslen m1
          folderFn pl_arr acc1 = firstGroup pl_arr (repl1,acc1)
      in foldr folderFn bs_str1 pl_arr_arr1



firstGroup::Replace_cl' a =>
    [PosLen] -> (a,a) -> a
firstGroup (pl1:_) r1@(repl1,bs_str1) = replaceMatch pl1 r1


-- | use in your custom 'GroupReplacer' passed to 'replaceGroup'
--
-- see example replacer above
--
replaceMatch::Replace_cl' a =>
    (R.MatchOffset, R.MatchLength) ->
        (a, a)  -- ^ (new val, acc passed to 'GroupReplacer')
        -> a
replaceMatch pl1 (repl1, bs_str1) =
     concat' [prefix1,repl1,suffix1]
     where   prefix1 = prefix pl1 bs_str1
             suffix1 = suffix pl1 bs_str1


{- | == dynamic group replace
   custom replacer fn returns replacement value

   >>> replacer::GroupReplacer String
       replacer marr1 acc1 = case val1 of
                              "101" -> fn1 "[A]"
                              "3" -> fn1 "[Be]"
       where ol1 = marr1 ! 3 :: (MatchOffset, MatchLength)
             val1 = extract ol1 acc1
             fn1 str1 = replaceMatch ol1 (str1,acc1)

    see 'extract'

    below test compares 'Once' vs 'All' options

    >>> groupReplace::IO()
        groupReplace =  hspec $ do
            describe "Pcre.Replace group" $ do
                it "Once" $ do
                   runFn1 [Once] `shouldBe` "a=[A] b=3 12"
                it "All" $ do
                   runFn1 [All] `shouldBe` "a=[A] b=[Be] 12"
                where runFn1 opts1 =
                         let   rx1 = Needle "(\\w)(=)(\\d{1,3})"
                               body1 = Haystack "a=101 b=3 12"
                         in replaceGroup opts1 (rx1,replacer) body1
-}
replaceGroup::Mm a =>
    [ReplaceCase]->(Needle a,GroupReplacer a) -> Haystack a -> a
replaceGroup cases (pat1,repl1) = fn1 (pat2,repl1)
    where pat2 = addOpt pat1 cOpt
          cOpt = comp cases
          fn1 = if elem All cases
                 then rallGroup
                 else ronceGroup


ronceGroup::Match_cl Regex a =>
    (Needle Regex, GroupReplacer a) -> Haystack a -> a
ronceGroup (pat1, repl1) body1@(Haystack bs_str1) =
     let m1 = match pat1 body1
     in case m1 of
            Nothing -> bs_str1
            Just marr1 -> repl1 marr1 bs_str1


rallGroup::Match_cl Regex a =>
    (Needle Regex, GroupReplacer a) -> Haystack a -> a
rallGroup (pat1, repl1) body1@(Haystack bs_str1) =
    let marrList1 = matchAll pat1 body1
    in foldr repl1 bs_str1 marrList1



class Replace_cl a where
   replace::[ReplaceCase] -> (Needle a, Replacement a) -> Haystack a -> a


class Replace_cl' a where
   prefix::PosLen -> a -> a
   suffix::PosLen -> a -> a
   concat'::[a] -> a


{- |
    >>> replace [Once,Utf8] (Needle "поп", Replacement  "крестьянин") (Haystack "у попа была собака")

    "у крестьянина была собака"

    >>> replace [Once,Utf8] (Needle "^a\\s", Replacement "A") (Haystack "a bc хол.гор.")

    "Abc хол.гор."

 -}

instance Replace_cl String where
   replace::[ReplaceCase] -> (Needle String, Replacement String) -> Haystack String -> String
   replace cases0 (pat1,repl1) hay0 = if isUtf8 cases0 then
        let res1 = fn1 (pat2 $ toByteString <$> pat1, toByteString <$> repl1) $ toByteString <$> hay0
        in toString res1
        else fn2 (pat2 pat1,repl1) hay0
        where pat2 pat1 = addOpt pat1 cOpt
              cOpt = comp cases0
              fn1 = if elem All cases0 then rall else ronce
              fn2 = if elem All cases0 then rall else ronce


instance Replace_cl' String where
   prefix pl1 = take $ fst pl1
   suffix pl1 = drop (pos1 + len1)
      where pos1 = fst pl1
            len1 = snd pl1
   concat' = concat



instance Replace_cl B.ByteString where
   replace::[ReplaceCase] -> (Needle B.ByteString, Replacement B.ByteString) ->
        Haystack B.ByteString -> B.ByteString
   replace cases0 (pat1,repl1) hay0 = fn1 (pat2,repl1) hay0
    where pat2 = addOpt pat1 cOpt
          cOpt = comp cases0
          fn1 = if elem All cases0
                then rall
                else ronce


instance Replace_cl' B.ByteString where
   prefix pl1 = B.take $ fst pl1
   suffix pl1 = B.drop (pos1 + len1)
      where pos1 = fst pl1
            len1 = snd pl1
   concat' = B.concat


comp::[ReplaceCase]-> [O.Comp]
comp = map mapFn . filter filterFn
   where filterFn o1 = o1 `elem` [Utf8,Multiline]
         mapFn Utf8 = O.Utf8
         mapFn Multiline = O.Multiline


isUtf8::[ReplaceCase] -> Bool
isUtf8 case0 = Utf8 `elem` case0