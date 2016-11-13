{- | see also "Text.Regex.Do.Pcre.Ascii.Replace"

    'Pattern' & 'Body' are wrapped in 'Utf8_' encoding tag.
    This tag adds clarity, prevents calling Ascii functions by mistake.

    __'toByteString''__ converts String to 'Utf8_' 'ByteString'     -}

module Text.Regex.Do.Pcre.Utf8.Replace
    (Replace(..),
    Replace'())  where

import Prelude as P
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Pcre.Matchf
import qualified Text.Regex.Do.ReplaceOpen as O
import Text.Regex.Do.Type.Regex as T
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Convert
import Data.ByteString

{- | see "Text.Regex.Do.Pcre.Ascii.Replace" for implemented types

    in full typed instance every b is wrapped in 'Utf8_' newtype

    'GroupReplacer' is implemented only for 'ByteString'    -}

class Replace pat repl body out where
    replace::pat -> repl -> body -> out


instance (Replace' all enc b repl) =>
    Replace (all (Pattern (enc b))) (repl (enc b)) (Body (enc b)) b where
    replace = replace'
{- ^ full typed arg

    >>> replace (Once $ Pattern $ Utf8_ "праздник")
            (Replacement $ Utf8_ "радость")
            (Body $ Utf8_ "экзамен - всегда праздник")
-}


instance (T.Regex b, Hint all,
        Replace' all Utf8_ b repl,
        Functor all,
        Enc' repl Utf8_) =>
    Replace (all b) (repl b) b b where
    replace p0 r0 b0 = replace' p1 (enc' r0) $
            Body $ Utf8_ b0
            where p1 = Pattern  . Utf8_ <$> p0
{- ^ hint 'Pattern'

    >>> replace (All "праздник")
            (Replacement "радость")
            "экзамен - всегда праздник"
    -}


instance (T.Regex b, Hint all,
        Replace' all Utf8_ b repl,
        Functor all,
        Enc' repl Utf8_) =>
    Replace b (all (repl b)) b b where
    replace p0 r0 b0 = replace' p1 (enc' $ unhint r0) $
            Body $ Utf8_ b0
        where p1 = swap r0 $ Pattern $ Utf8_ p0
{- ^ hint repl

    >>> replacer::GroupReplacer (ByteString)
    replacer = defaultReplacer 1 tweak1
          where tweak1 s1
                    | s1 == toByteString "[команды]" = toByteString "А - Я"
                    | s1 == toByteString "[счёт]" = toByteString "5:0"
                    | s1 == toByteString "[какая боль, ]" = empty
                    | otherwise = traceShow s1 $ toByteString "?"

    >>> let rx1 = toByteString "(\\[[^\\]]+\\])"
            body1 = toByteString "[какая боль, ][команды] : [счёт]"
        in replace rx1 (All replacer) body1

    "А - Я : 5:0"       -}

instance (T.Regex b,
        Replace' Once Utf8_ b repl,
        Enc' repl Utf8_) =>
    Replace b (repl b) (Once b) b where
    replace p0 r0 b0 = replace' p1 (enc' r0) $
            Body $ Utf8_ $ unhint b0
        where p1 = swap b0 $ Pattern $ Utf8_ p0
{- ^ hint 'Body'

    >>> replace "праздник"
            (Replacement "радость")
            (Once "экзамен - всегда праздник")
    -}


instance (T.Regex b,
        Replace' All Utf8_ b repl,
        Enc' repl Utf8_) =>
    Replace b (repl b) (All b) b where
    replace p0 r0 b0 = replace' p1 (enc' r0) $
            Body $ Utf8_ $ unhint b0
        where p1 = swap b0 $ Pattern $ Utf8_ p0
{- ^ hint 'Body'

    >>> replace "праздник"
            (Replacement "радость")
            (All "экзамен - всегда праздник")
    -}



{- | internal class & instances

    use 'replace' instead  -}
class Replace' all enc a repl where
    replace'::all (Pattern (enc a)) -> repl (enc a) -> Body (enc a) -> a


instance Replace' Once Utf8_ String Replacement where
    replace' = replace_str marray_

instance Replace' All Utf8_ String Replacement where
    replace' = replace_str marray_

replace_str f0 p0 r0 b0 =
   let ma1 = f0 p1 b1
       b1 = toByteString . val <$> b0
       r1 = toByteString <$> (val <$> r0)
       p1 = T.makeRegex' . (toByteString' . val <$>) <$> p0
   in toString $ O.replace ma1 r1 b1


instance Replace' Once Utf8_ ByteString Replacement where
    replace' = replace_bs marray_

instance Replace' All Utf8_ ByteString Replacement where
    replace' = replace_bs marray_

replace_bs f0 p0 r0 b0 =
   let ma1 = f0 p1 b1
       b1 = val <$> b0
       p1 = T.makeRegex' . (Utf8_ . val <$>) <$> p0
   in O.replace ma1 (val <$> r0) b1



instance Replace' Once Utf8_ ByteString GroupReplacer where
    replace' = replace_bs' marray_

instance Replace' All Utf8_ ByteString GroupReplacer where
    replace' = replace_bs' marray_

{- ^ >>> replacer::GroupReplacer (Utf8_ ByteString)
         replacer = defaultReplacer 1 tweak1
          where tweak1 bs1 = toByteString' $
                                if bs1 == toByteString' "左" then
                                      "ー右ー"
                                       else "?"


     >>> runFn1 `shouldBe` toByteString "100メートルー右ー折後、左"
            where runFn1 =
                     let rx1 = Pattern $ toByteString' "(?<=ル)(左)"
                         body1 = Body $ toByteString' "100メートル左折後、左"
                     in replace (All rx1) replacer body1        -}


replace_bs' f0 p0 r0 b0 =
   let ma1 = f0 p1 b1
       b1 = val <$> b0
       p1 = T.makeRegex' . (Utf8_ . val <$>) <$> p0
   in O.replace ma1 (val' r0) b1


{- ^ to tweak regex with 'Comp' or 'Exec', see "Text.Regex.Do.Type.Regex"

    note 'Once_Utf8' and 'All_Utf8' hints   -}
