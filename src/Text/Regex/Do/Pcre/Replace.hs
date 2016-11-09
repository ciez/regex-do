module Text.Regex.Do.Pcre.Replace(
   ReplaceCase(..),
   Replace(..),
   replaceMatch,
   defaultReplacer,
   getGroup,
   Mr_
   )  where


import Data.Array as A
import Prelude as P
import Data.ByteString as B
import qualified Text.Regex.Do.Pcre.Option as O
import qualified Text.Regex.Base.RegexLike as R
import Text.Regex.Do.Convert
import Text.Regex.Do.Pcre.Match as M
import Text.Regex.Do.Pcre.Result as R
import Text.Regex.Do.TypeDo
import Text.Regex.Do.TypeRegex
import Text.Regex.Do.Pcre.Matchf


type Mr_ a out = (Match a a out, Rx_ a a, Replace_ a, Opt_ a)


class Mr_ a [a] => Replace a where
   replace::[ReplaceCase] -> Pattern a -> Replacement a -> Body a -> a
   replace cases0 p0 repl0 b0 =
        if isUtf8 cases0 then utfFn2
        else fn2 (pat2 p0,repl0) b0
        where fn1 = if P.elem All cases0 then rall else ronce
              fn2 = if P.elem All cases0 then rall else ronce
              utfFn2 = let res1 = fn1 (pat2 $ toByteString' <$> p0, toByteString' <$> repl0) $ toByteString' <$> b0
                       in toA res1
              pat2 p0 = addOpt p0 cOpt1
              cOpt1 = comp cases0


   replaceGroup::[ReplaceCase] -> Pattern a -> GroupReplacer a -> Body a -> a
   replaceGroup cases0 p0 repl0 = fn1 pat2 repl0
        where pat2 = addOpt p0 cOpt
              cOpt = comp cases0
              fn1 = if P.elem All cases0
                     then rallGroup
                     else ronceGroup

{- ^
   == dynamic group replace
   custom replacer fn returns replacement value

   >>> replacer::GroupReplacer String
       replacer = defaultReplacer 1 tweak1
             where tweak1 str1 = case str1 of
                                   "101" -> "[сто один]"
                                   "3" -> "[three]"
                                   otherwise -> trace str1 "?"

    'Once' vs 'All' options

    >>> replaceGroup [Once,Utf8] (Pattern "\\w=(\\d{1,3})", replacer) $ Body "a=101 b=3 12"

        "a=[сто один] b=3 12"

    >>> replaceGroup [All,Utf8] (Pattern "\\w=(\\d{1,3})", replacer) $ Body "a=101 b=3 12"

        "a=[сто один] b=[three] 12"


    == static replace for simple (no group) needle

    >>> replace [Once,Utf8] (Pattern "менее", Replacement  "более") $ Body "менее менее"

    "более менее"

    >>> replace [Once,Utf8] (Pattern "^a\\s", Replacement "A") $ Body "a bc хол.гор."

    "Abc хол.гор."      -}


instance Replace String
instance Replace B.ByteString


class Replace_ a where
   prefix::PosLen -> a -> a
   suffix::PosLen -> a -> a
   concat'::[a] -> a
   len'::a -> Int
   toByteString'::a -> ByteString
   toA::ByteString -> a


instance Replace_ String where
   prefix pl0 = P.take $ fst pl0
   suffix pl0 = P.drop (pos1 + len1)
      where pos1 = fst pl0
            len1 = snd pl0
   concat' = P.concat
   toByteString' = toByteString
   toA = toString
   len' = P.length



instance Replace_ B.ByteString where
   prefix pl0 = B.take $ fst pl0
   suffix pl0 = B.drop (pos1 + len1)
      where pos1 = fst pl0
            len1 = snd pl0
   concat' = B.concat
   toByteString' = id
   toA = id
   len' = B.length


--  static
ronce::Mr_ a [a] =>
    (Pattern Regex, Replacement a) -> Body a -> a
ronce (p0, Replacement repl0) h1@(Body h0) =
      let pl2 = let m1 = marray_ p0 h1
                in R.poslen m1
      in case pl2 of
         Nothing -> h0
         Just lpl1 -> firstGroup lpl1 (repl0, h0)


rall::Mr_ a [a] =>
    (Pattern Regex, Replacement a) -> Body a -> a
rall (p0, Replacement repl0) h1@(Body h0) =
      let lpl1 = let m1 = marray_ p0 h1
                 in R.poslen m1::[[PosLen]]
          foldFn1 lpl1 acc1 = firstGroup lpl1 (repl0,acc1)
      in P.foldr foldFn1 h0 lpl1


firstGroup::(Replace_ a) =>
    [PosLen] -> (a,a) -> a
firstGroup (pl0:_) r1@(new0,a0) = acc $ replaceMatch pl0 (new0, acc1)
    where acc1 = ReplaceAcc {
                    acc = a0,
                    pos_adj = 0
                    }


--  dynamic
{- | you can write a custom replacer. This is only one common use case.

    Replaces specified (by idx) group match with tweaked value.     -}
defaultReplacer::(Replace_ a, R.Extract a) =>
        Int         -- ^ group idx
        -> (a -> a) -- ^ (group match -> replacement) tweak
            -> GroupReplacer a
defaultReplacer idx0 tweak0 (ma0::MatchArray) acc0 = maybe acc0 fn1 mval1
       where pl1 = ma0 A.! idx0 :: (R.MatchOffset, R.MatchLength)
             mval1 = getGroup acc0 ma0 idx0
             fn1 str1 = replaceMatch pl1 (str2, acc0)
                        where str2 = tweak0 str1


{- | get group content safely

    call from your custom 'GroupReplacer' passed to 'replaceGroup'
    -}
getGroup::R.Extract a =>
    ReplaceAcc a -> MatchArray -> Int -> Maybe a
getGroup acc0 ma0 idx0 = if idx0 >= P.length ma0 then Nothing     --  safety catch
    else Just val1
    where pl1 = ma0 A.! idx0 :: (R.MatchOffset, R.MatchLength)
          pl2 = adjustPoslen pl1 acc0
          val1 = extract pl2 $ acc acc0


adjustPoslen::PosLen -> ReplaceAcc a -> PosLen
adjustPoslen (p0,l0) acc0  = (p0 + pos_adj acc0, l0)


ronceGroup::Rx_ a a =>
    Pattern Regex -> GroupReplacer a -> Body a -> a
ronceGroup pat0 repl0 h1@(Body h0) =
     let m1 = marray_ pat0 h1::Maybe MatchArray
     in case m1 of
            Nothing -> h0
            Just ma1 -> let a1 = ReplaceAcc {
                                    acc = h0,
                                    pos_adj = 0
                                    }
                        in acc $ repl0 ma1 a1


rallGroup::Mr_ a [a] =>
    Pattern Regex -> GroupReplacer a -> Body a -> a
rallGroup pat0 repl0 b1@(Body b0) =
    let ma1 = marray_ pat0 b1::[MatchArray]
        acc1 = ReplaceAcc { acc = b0, pos_adj = 0 }
    in acc $ P.foldl (flip repl0) acc1 ma1


{- | call from your custom 'GroupReplacer' passed to 'replaceGroup'

     see example replacer above     -}
replaceMatch::Replace_ a =>
        PosLen      -- ^ replaceable, unadjusted
        -> (a, ReplaceAcc a)  -- ^ (new val, acc passed to 'GroupReplacer')
        -> ReplaceAcc a    -- ^ new acc
replaceMatch pl0@(_,l0) (new0, acc0) = ReplaceAcc {
                    acc = acc1,
                    pos_adj = pos_adj acc0 + l1 - l0
                    }
     where  pl1 = adjustPoslen pl0 acc0
            prefix1 = prefix pl1 $ acc acc0
            suffix1 = suffix pl1 $ acc acc0
            acc1 = concat' [prefix1, new0, suffix1]
            l1 = len' new0


addOpt::Opt_ a =>
    Pattern a -> [O.Comp] -> Pattern Regex
addOpt pat0 opt0 = Pattern rx1
    where rx1 = makeRegexOpts opt0 [] pat0


comp::[ReplaceCase]-> [O.Comp]
comp = P.map mapFn . P.filter filterFn
   where filterFn o1 = o1 `P.elem` [Utf8,Multiline]
         mapFn Utf8 = O.Utf8
         mapFn Multiline = O.Multiline


isUtf8::[ReplaceCase] -> Bool
isUtf8 case0 = Utf8 `P.elem` case0
