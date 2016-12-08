{- | for reliable results with Utf8 pattern or body,
    use "Text.Regex.Do.Replace.Utf8"   -}

module Text.Regex.Do.Replace.Latin
    (Replace(..))  where

import Text.Regex.Base.RegexLike as R
import Prelude as P
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Type.Reexport as R
import Text.Regex.Do.Match.Matchf
import qualified Text.Regex.Do.Replace.Open as O
import Text.Regex.Do.Match.Regex as T
import Text.Regex.Do.Type.Extract
import Text.Regex.Do.Type.MatchHint


{- | hint: 'All' | 'Once'

    pattern: 'R.Regex', 'String', 'ByteString'   
    
    String | ByteString pattern may contains regex
    
    body: 'String', 'ByteString'

    result is 'Either' String body: 'Left' String returns regex construction errors.   
-} 
class Replace hint pattern repl body out where
    replace::(Extract' body, RegexLike R.Regex body) => 
        hint pattern -> repl -> body -> out


instance (R.RegexLike R.Regex b, O.Replace Maybe repl b) =>
        Replace Once R.Regex repl b b where
    replace (Once p0) = replace_rx p0 tagOnce
{- ^ succeeds unless 'GroupReplacer' fails due to mismatched pattern etc 

    repl: 'String' | 'ByteString' | 'GroupReplacer' repl
-}


instance (R.RegexLike R.Regex b, O.Replace [] repl b) =>
        Replace All R.Regex repl b b where
    replace (All p0) = replace_rx p0 tagAll
{- ^ succeeds unless 'GroupReplacer' fails due to mismatched pattern etc 

    repl: 'String' | 'ByteString' | 'GroupReplacer' repl
-}



instance (R.RegexLike R.Regex b, T.Regex b) => 
    Replace Once b b b (E b) where
    replace (Once p0) = replace_ p0 tagOnce
{- ^ b: 'String' | 'ByteString'-}


instance (R.RegexLike R.Regex b, T.Regex b) => 
    Replace Once b (GroupReplacer b) b (E b) where
    replace (Once p0) = replace_ p0 tagOnce
{- ^ b: 'String' | 'ByteString'-}


instance (R.RegexLike R.Regex b, T.Regex b) => 
    Replace All b b b (E b) where
    replace (All p0) = replace_ p0 tagAll
{- ^ b: 'String' | 'ByteString'-}


instance (R.RegexLike R.Regex b, T.Regex b) => 
    Replace All b (GroupReplacer b) b (E b) where
    replace (All p0) = replace_ p0 tagAll
{- ^ b: 'String' | 'ByteString'-}


replace_rx rx0 tag0 r0 b0 =
    let Right ma1 = marray_ (Right rx0) (tag0 b0) 
    in O.replace ma1 r0 b0


replace_ p0 tag0 r0 b0 = 
    marray_ (T.makeRegex p0) (tag0 b0) >>= \ma1 ->
    Right $ O.replace ma1 r0 b0

