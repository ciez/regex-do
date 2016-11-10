module Text.Regex.Do.Pcre.Replace(
    replace,
    ReplaceCase_(..))  where

import Text.Regex.Base.RegexLike as R
import Prelude as P
import Data.ByteString as B
import qualified Text.Regex.Do.Pcre.Option as O
import Text.Regex.Do.Pcre.Match as M
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Type.Reexport
import Text.Regex.Do.Pcre.Matchf
import qualified Text.Regex.Do.Pcre.ReplaceOpen as O
import Text.Regex.Do.Convert
import Text.Regex.Do.Type.Regex_
import Text.Regex.Do.Type.Extract


{- | == dynamic group replace
   custom replacer fn returns replacement value. See 'O.defaultReplacer'

   >>> replacer::GroupReplacer String
       replacer = defaultReplacer 1 tweak1
             where tweak1 str1 = case str1 of
                                   "101" -> "[сто один]"
                                   "3" -> "[three]"
                                   otherwise -> trace str1 "?"

    'Once' vs 'All' options

    >>> replace [Once,Utf8] (Pattern "\\w=(\\d{1,3})") replacer $ Body "a=101 b=3 12"

        "a=[сто один] b=3 12"

    >>> replace [All,Utf8] (Pattern "\\w=(\\d{1,3})") replacer $ Body "a=101 b=3 12"

        "a=[сто один] b=[three] 12"


    == static replace for simple (no group) needle

    for no-regex 'ByteString' replacement see "Text.Regex.Do.Split"

    >>> replace [Once,Utf8] (Pattern "менее") (Replacement  "более") $ Body "менее менее"

    "более менее"

    >>> replace [Once,Utf8] (Pattern "^a\\s") (Replacement "A") $ Body "a bc хол.гор."

    "Abc хол.гор."      -}


replace::(Rx_ a a, ReplaceCase_ r a, Opt_ a) =>
    [ReplaceCase] -> Pattern a -> r a -> Body a -> a
replace case0 p0 r0 b0 = replace_ case0 p1 r0 b0
    where opt1 = comp case0
          p1 = addOpt p0 opt1


-- | implementation detail for the curious
class ReplaceCase_ r a where
    replace_::[ReplaceCase] -> Pattern Regex -> r a -> Body a -> a

instance ReplaceCase_ Replacement String where
    replace_ case0 p0 r0 b0 =
        let ma1 = marray_ p0 b1::Maybe MatchArray
            ma2 = marray_ p0 b1::[MatchArray]
            b1 = toByteString <$> b0
            r1 = toByteString <$> r0
            isAll1 = All `P.elem` case0
            isUtf1 = Utf8 `P.elem` case0
            bs1 = if isAll1 then O.replace ma2 r1 b1
                  else O.replace ma1 r1 b1
        in if isUtf1 then toString bs1
            else vanilla_replace case0 p0 r0 b0


instance ReplaceCase_ Replacement ByteString where
    replace_ = vanilla_replace

instance ReplaceCase_ GroupReplacer ByteString where
    replace_ = vanilla_replace

instance ReplaceCase_ GroupReplacer String where
    replace_  = vanilla_replace


vanilla_replace::(O.ReplaceOpen [] r, O.ReplaceOpen Maybe r,
    Extract' a,Rx_ a a,Opt_ a) =>
        [ReplaceCase] -> Pattern Regex -> r a -> Body a -> a
vanilla_replace case0 p0 r0 b0 =
   let ma1 = marray_ p0 b0::Maybe MatchArray
       ma2 = marray_ p0 b0:: [MatchArray]
       isAll1 = All ` P.elem ` case0
   in if isAll1 then O.replace ma2 r0 b0
              else O.replace ma1 r0 b0


addOpt::Opt_ a =>
    Pattern a -> [O.Comp] -> Pattern Regex
addOpt pat0 opt0 = Pattern rx1
    where rx1 = M.makeRegexOpts opt0 [] pat0


comp::[ReplaceCase]-> [O.Comp]
comp = P.map mapFn . P.filter filterFn
   where filterFn o1 = o1 `P.elem` [Utf8,Multiline]
         mapFn Utf8 = O.Utf8
         mapFn Multiline = O.Multiline
