-- | reexport common types from "Text.Regex.PCRE"
module Text.Regex.Do.TypeRegex (
    W.Regex(..),
    R.MatchArray(..),
    W.CompOption(..),
    W.ExecOption()
    )   where

import Text.Regex.PCRE.ByteString as B (Regex)
import Text.Regex.PCRE.String as S (Regex)
import Text.Regex.Base.RegexLike as R (MatchArray)
import Text.Regex.PCRE.Wrap as W

type RegexB = B.Regex
type RegexS = S.Regex
