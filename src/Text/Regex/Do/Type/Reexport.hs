-- | reexport common types from "Text.Regex.PCRE"
module Text.Regex.Do.Type.Reexport (
    W.Regex(..) -- | "Text.Regex.PCRE.Wrap" 
    ,R.MatchArray(..) -- | "Text.Regex.Base.RegexLike" 
    ,W.CompOption(..) -- | "Text.Regex.PCRE.Wrap"
    ,W.ExecOption()   -- | "Text.Regex.PCRE.Wrap"
    )   where

import Text.Regex.PCRE.ByteString as B (Regex)
import Text.Regex.PCRE.String as S (Regex)
import Text.Regex.Base.RegexLike as R (MatchArray)
import Text.Regex.PCRE.Wrap as W

type RegexB = B.Regex
type RegexS = S.Regex
