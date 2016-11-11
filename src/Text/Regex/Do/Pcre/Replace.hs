module Text.Regex.Do.Pcre.Replace
    (Replace(..))  where

import Text.Regex.Base.RegexLike as R
import Prelude as P
import Data.ByteString as B
import qualified Text.Regex.Do.Pcre.Option as O
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Type.Reexport
import Text.Regex.Do.Pcre.Matchf
import qualified Text.Regex.Do.Pcre.ReplaceOpen as O
import Text.Regex.Do.Convert
import Text.Regex.Do.Type.Regex_
import Text.Regex.Do.Type.Extract
import Text.Regex.Do.Type.MatchHint



type Ro_ rx = (Regex_ rx, Opt_ rx)

rx'::Regex_ rx => Pattern rx -> [O.Comp] -> Pattern Regex
rx' p0 opt0 = Pattern $ ropt_ p0 opt0 []


{- | == dynamic group replace
   custom replacer fn returns replacement value. See 'O.defaultReplacer'

   >>> replacer::GroupReplacer String
       replacer = defaultReplacer 1 tweak1
             where tweak1 str1 = case str1 of
                                   "101" -> "[сто один]"
                                   "3" -> "[three]"
                                   otherwise -> trace str1 "?"

    'Once' vs 'All' options

    >>> replace (Once[]) (Pattern "\\w=(\\d{1,3})") replacer $ Body "a=101 b=3 12"

        "a=[сто один] b=3 12"

    >>> replace (All[]) (Pattern "\\w=(\\d{1,3})") replacer $ Body "a=101 b=3 12"

        "a=[сто один] b=[three] 12"


    == static replace for simple (no group) needle

    for no-regex 'ByteString' replacement see "Text.Regex.Do.Split"

    >>> replace (Once[Utf8]) (Pattern "менее") (Replacement  "более") $ Body "менее менее"

    "более менее"

    >>> replace (Once[]) (Pattern "^a\\s") (Replacement "A") $ Body "a bc хол.гор."

    "Abc хол.гор."      -}

class Replace hint rx r a where
    replace::hint [O.Comp] -> Pattern rx -> r a -> Body a -> a


instance Regex_ rx => Replace Once rx Replacement String where
    replace opt0@(Once opt1) p0 r0 b0 =
        let ma1 = marray_ p1 b1::Maybe MatchArray
            b1 = toByteString <$> b0
            r1 = toByteString <$> r0
            isUtf1 = O.Utf8 `P.elem` opt1
            bs1 = O.replace ma1 r1 b1
            p1 = rx' p0 opt1
        in if isUtf1 then toString bs1
            else vanilla_once p1 r0 b0


instance Regex_ rx => Replace All rx Replacement String where
    replace opt0@(All opt1) p0 r0 b0 =
        let ma2 = marray_ p0 b1::[MatchArray]
            b1 = toByteString <$> b0
            r1 = toByteString <$> r0
            isUtf1 = O.Utf8 `P.elem` opt1
            bs1 = O.replace ma2 r1 b1
            p1 = rx' p0 opt1
        in if isUtf1 then toString bs1
            else vanilla_all p1 r0 b0


instance Regex_ rx => Replace Once rx Replacement ByteString where
    replace opt0@(Once opt1) p0 = vanilla_once $ rx' p0 opt1

instance Regex_ rx => Replace All rx Replacement ByteString where
    replace opt0@(All opt1) p0 = vanilla_all $ rx' p0 opt1

instance Regex_ rx => Replace Once rx GroupReplacer ByteString where
    replace opt0@(Once opt1) p0 = vanilla_once $ rx' p0 opt1

instance Regex_ rx => Replace All rx GroupReplacer ByteString where
    replace opt0@(All opt1) p0 = vanilla_all $ rx' p0 opt1

instance Regex_ rx => Replace Once rx GroupReplacer String where
    replace opt0@(Once opt1) p0 = vanilla_once $ rx' p0 opt1

instance Regex_ rx => Replace All rx GroupReplacer String where
    replace opt0@(All opt1) p0 = vanilla_all $ rx' p0 opt1


type Vanilla_ f r a = (O.ReplaceOpen f r, Extract' a, RegexLike Regex a)
type Vanilla_once r a = Vanilla_ Maybe r a
type Vanilla_all r a = Vanilla_ [] r a


vanilla_once::Vanilla_once r a =>
        Pattern Regex ->
        r a ->
        Body a ->
            a
vanilla_once p0 r0 b0 =
   let ma1 = marray_ p0 b0::Maybe MatchArray
   in O.replace ma1 r0 b0


vanilla_all::Vanilla_all r a =>
        Pattern Regex ->
        r a ->
        Body a ->
            a
vanilla_all p0 r0 b0 =
   let ma2 = marray_ p0 b0:: [MatchArray]
   in O.replace ma2 r0 b0
