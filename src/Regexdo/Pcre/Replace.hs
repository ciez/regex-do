module Regexdo.Pcre.Replace(
   ReplaceCase(..),
   Replace_cl(..),
   replaceMatch,
   defaultReplacer,
   Mr
   )  where

import Data.Array
import Data.ByteString
import Prelude as P
import qualified Data.ByteString as B
import qualified Regexdo.Pcre.Option as O
import qualified Text.Regex.Base.RegexLike as R
import Regexdo.Convert
import Regexdo.Pcre.Match
import Regexdo.Pcre.Result
import Regexdo.TypeDo
import Regexdo.TypeRegex


type Mr a = (Match_cl Regex a, Replace_cl' a, Match_opt a)


class Replace_cl' a where
   prefix::PosLen -> a -> a
   suffix::PosLen -> a -> a
   concat'::[a] -> a
   toByteString'::a -> ByteString
   toA::ByteString -> a


class Replace_cl a where
   replace::Mr a =>
    [ReplaceCase] -> (Needle a, Replacement a) -> Haystack a -> a
   replace cases0 (pat1,repl1) hay0 =
        if isUtf8 cases0 then utfFn2
        else fn2 (pat2 pat1,repl1) hay0
        where fn1 = if P.elem All cases0 then rall else ronce
              fn2 = if P.elem All cases0 then rall else ronce
              utfFn2 = let res1 = fn1 (pat2 $ toByteString' <$> pat1, toByteString' <$> repl1) $ toByteString' <$> hay0
                       in toA res1
              pat2 pat1 = addOpt pat1 cOpt1
              cOpt1 = comp cases0


   replaceGroup::Mr a =>
        [ReplaceCase] -> (Needle a, GroupReplacer a) -> Haystack a -> a
   replaceGroup cases (pat1,repl1) = fn1 (pat2,repl1)
        where pat2 = addOpt pat1 cOpt
              cOpt = comp cases
              fn1 = if P.elem All cases
                     then rallGroup
                     else ronceGroup

{- ^
   == dynamic group replace
   custom replacer fn returns replacement value

   >>> replacer::GroupReplacer String
       replacer = defaultReplacer 3 tweak1
             where tweak1 str1 = case str1 of
                                  "101" -> "[A]"
                                  "3" -> "[Be]"


    'Once' vs 'All' options

    >>> replaceGroup [Once] (Needle "(\\w)(=)(\\d{1,3})", replacer) $ Haystack "a=101 b=3 12"

        "a=[A] b=3 12"

    >>> replaceGroup [All] (Needle "(\\w)(=)(\\d{1,3})", replacer) $ Haystack "a=101 b=3 12"

        "a=[A] b=[Be] 12"


    == static replace for simple (no group) needle

    >>> replace [Once,Utf8] (Needle "менее", Replacement  "более") (Haystack "менее менее")

    "более менее"

    >>> replace [Once,Utf8] (Needle "^a\\s", Replacement "A") (Haystack "a bc хол.гор.")

    "Abc хол.гор."      -}

instance Replace_cl String

instance Replace_cl' String where
   prefix pl1 = P.take $ fst pl1
   suffix pl1 = P.drop (pos1 + len1)
      where pos1 = fst pl1
            len1 = snd pl1
   concat' = P.concat
   toByteString' = toByteString
   toA = toString


instance Replace_cl B.ByteString

instance Replace_cl' B.ByteString where
   prefix pl1 = B.take $ fst pl1
   suffix pl1 = B.drop (pos1 + len1)
      where pos1 = fst pl1
            len1 = snd pl1
   concat' = B.concat
   toByteString' = id
   toA = id


-- | use in your custom 'GroupReplacer' passed to 'replaceGroup'
--
-- see example replacer above or use 'defaultReplacer'
--
replaceMatch::Replace_cl' a =>
        (R.MatchOffset, R.MatchLength) ->
        (a, a)  -- ^ (new val, acc passed to 'GroupReplacer')
        -> a    -- ^ new acc
replaceMatch pl1 (repl1, bs_str1) =
     concat' [prefix1,repl1,suffix1]
     where   prefix1 = prefix pl1 bs_str1
             suffix1 = suffix pl1 bs_str1

--  static
ronce::Mr a =>
    (Needle Regex, Replacement a) -> Haystack a -> a
ronce (pat1, Replacement repl1) body1@(Haystack bs_str1) =
      let pl2 = do
               let m1 = match pat1 body1
               poslen m1
      in case pl2 of
         Nothing -> bs_str1
         Just pl_arr -> firstGroup pl_arr (repl1,bs_str1)


rall::Mr a =>
    (Needle Regex, Replacement a) -> Haystack a -> a
rall (pat1, Replacement repl1) body1@(Haystack bs_str1) =
      let pl_arr_arr1 = do
               let m1 = matchAll pat1 body1
               poslen m1
          folderFn pl_arr acc1 = firstGroup pl_arr (repl1,acc1)
      in P.foldr folderFn bs_str1 pl_arr_arr1


firstGroup::Replace_cl' a =>
    [PosLen] -> (a,a) -> a
firstGroup (pl1:_) r1@(repl1,bs_str1) = replaceMatch pl1 r1


--  dynamic
{- | replace with a tweak to specified (by idx) group match

    see 'defaultReplacer' source for hints: how to write custom replacer
     -}
defaultReplacer::(Replace_cl' a, R.Extract a) =>
        Int         -- ^ idx of match within a group
        -> (a -> a) -- ^ (group match -> replacement) tweak
            -> GroupReplacer a
defaultReplacer idx0 tweak0 (ma0::MatchArray) acc0 =
        if idx0 >= P.length ma0 then acc0     --  safety catch
        else fn1 val1
       where poslen1 = ma0 ! idx0 :: (R.MatchOffset, R.MatchLength)
             val1 = extract poslen1 acc0
             fn1 str1 = replaceMatch poslen1 (str2,acc0)
                        where str2 = tweak0 str1


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
    in P.foldr repl1 bs_str1 marrList1



addOpt::Match_opt a =>
    Needle a -> [O.Comp] -> Needle Regex
addOpt pat1 opt1 = Needle rx1
    where rx1 = makeRegexOpts opt1 [] pat1


comp::[ReplaceCase]-> [O.Comp]
comp = P.map mapFn . P.filter filterFn
   where filterFn o1 = o1 `P.elem` [Utf8,Multiline]
         mapFn Utf8 = O.Utf8
         mapFn Multiline = O.Multiline


isUtf8::[ReplaceCase] -> Bool
isUtf8 case0 = Utf8 `P.elem` case0