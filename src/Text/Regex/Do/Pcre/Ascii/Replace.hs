{- | === Ascii vs Utf8 modules

    for reliable results with Utf8 pattern or body,
    use "Text.Regex.Do.Pcre.Utf8.Replace"   -}

module Text.Regex.Do.Pcre.Ascii.Replace
    (Replace(..),
    Repl_)  where

import Text.Regex.Base.RegexLike as R
import Prelude as P
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Type.Reexport as R
import Text.Regex.Do.Pcre.Matchf
import qualified Text.Regex.Do.ReplaceOpen as O
import Text.Regex.Do.Type.Regex as T
import Text.Regex.Do.Type.Extract
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Pcre.Option as O


class Replace all a repl b where
    replace::(R_ b, Matchf all b) =>
        all (Pattern a) -> repl b -> Body b -> b

type Repl_ f rx r a = (T.Regex rx,
                        R.RegexLike R.Regex a,
                        Extract' a,
                        O.ReplaceOpen f r)

replace_ fn0 p0 r0 b0 =
   let ma1 = fn0 p1 b0
       p1 = T.makeRegex' <$> p0
   in O.replace ma1 r0 b0



instance Repl_ Maybe a repl b => Replace Once a repl b where
    replace = replace_ marray_

{- ^ === static replace for simple (no group) needle

    for no-regex 'ByteString' replacement see "Text.Regex.Do.Split"

    >>> replace (Once (Pattern "^a\\s")) (Replacement "A") (Body "a bc")

    \"Abc\"

   === dynamic group replace

    >>> replace (Once (Pattern "\\w=(\\d{1,3})")) replacer $ Body "a=101 b=3 12"

     "a=[1 0 1] b=3 12"     -}


instance Repl_ [] a repl b => Replace All a repl b where
    replace = replace_ marray_


{- ^ to tweak regex with 'O.Comp' or 'O.Exec', see "Text.Regex.Do.Type.Regex"

   === dynamic group replace

   custom replacer fn returns replacement value. See 'O.defaultReplacer'

   >>> replacer::GroupReplacer String
       replacer = defaultReplacer 1 tweak1
             where tweak1 str1 = case str1 of
                                   "101" -> "[1 0 1]"
                                   "3" -> "[ 3 ]"
                                   otherwise -> trace str1 "?"


    >>> replace (All (Pattern "\\w=(\\d{1,3})")) replacer $ Body "a=101 b=3 12"

        "a=[1 0 1] b=[ 3 ] 12"      -}

dum_::Comp
dum_ = Blank
