{- | although sometimes funs in Ascii modules work with non-ascii text
    (as some examples show),
    for reliable results with Utf8 pattern or body,
    use "Text.Regex.Do.Pcre.Match.Utf8"

    see also "Text.Regex.Base.RegexLike" -}

module Text.Regex.Do.Match.Latin
    (MatchOnce(..),
    MatchAll(..),
    R.extract   -- | 'extract' is reexport from "Text.Regex.Base.RegexLike"
    ) where

import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Match.Matchf as F
import Text.Regex.PCRE.Wrap()
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Match.Regex as R
import Text.Regex.Do.Type.Reexport as Re
import Data.Tagged


{- | === API changes: 
    
    Once is hinted with '~?' 
    
    All is hinted with '~*'   

    All regex-computing instances catch regex errors, return 'Either' 'String' out ('Left' String is the error message)
    
    * pattern:  'String', 'ByteString', 'Regex'
    
        String | ByteString pattern may contains regex

    * body:  'String', 'ByteString'
    * out: out | E out

        * 'Bool': test if regex matches
        * ['String']
        * ['ByteString']
        * ['PosLen']


    precompiled Regex may be used as pattern too. see "Text.Regex.Do.Pcre.Match.Utf8"  -}
class MatchOnce pattern body out where
    (~?)::pattern -> body -> out

{- | * pattern:  'String', 'ByteString', 'Regex'
    * body:  'String', 'ByteString'
    * out: out | E out

        * [['String']]
        * [['ByteString']]
        * [['PosLen']]
-}
class MatchAll pattern body out where
    (~*)::pattern -> body -> out


instance R.RegexLike Re.Regex b => MatchOnce Re.Regex b [b]  where
    (~?) pat0 body0 = once pat0 (Tagged body0)
-- ^ always succeeds               


instance (R.Regex b, R.RegexLike Re.Regex b) => 
        MatchOnce b b (E [b])  where     
    (~?) pat0 body0 = withRegex pat0 (Tagged body0) once
{- ^  >>> "^all" ~? "all the time"::E [String]
        
     Right \["all"]      -}

instance (R.Regex b, R.RegexLike Re.Regex b) =>
        MatchOnce b b (E Bool) where
    (~?) pat0 body0 = withRegex pat0 (tagB body0) test
        where tagB::b -> Tagged Test b
              tagB = Tagged
{- ^ test

    >>> "chilly" ~? "it's chilly inside, chilly outside"::E Bool

    Right True    -}
              

instance R.RegexLike Re.Regex b =>
        MatchOnce Re.Regex b Bool where
    (~?) pat0 body0 = test pat0 (tagB body0) 
        where tagB::b -> Tagged Test b
              tagB = Tagged    
{- ^ test.

always succeeds -}               


instance R.RegexLike Re.Regex b =>
    MatchAll Re.Regex b [[b]] where
    (~*) pat0 body0 = F.all pat0 (Tagged body0)
-- ^ always succeeds               


instance (R.Regex b, R.RegexLike Re.Regex b) =>
        MatchAll b b (E [[b]]) where
    (~*) pat0 body0 = withRegex pat0 (Tagged body0) F.all 
{- ^ @ 
"chilly" ~* "it's chilly inside, chilly outside"::E [[ByteString]]

Right \[\["chilly"],["chilly"]]   
     @     -}


instance R.RegexLike Re.Regex b =>
        MatchOnce Re.Regex b [PosLen] where
    (~?) pat0 body0 = r1 
      where tagOne::b -> Tagged Once b
            tagOne = Tagged
            Right r1 = poslen_ (Right pat0) (tagOne body0)
-- ^ always succeeds               


instance (R.Regex b, R.RegexLike Re.Regex b) =>
        MatchOnce b b (E [PosLen]) where
    (~?) pat0 body0 = withRegex' pat0 (tagOne body0) poslen_
      where tagOne::b -> Tagged Once b
            tagOne = Tagged
      
{-^ >>> "à" ~? "tourner à gauche"::E [PosLen]

  Right \[(8,2)]     -}


instance R.RegexLike Re.Regex b =>
        MatchAll Re.Regex b [[PosLen]] where
    (~*) pat0 body0 = r1 
      where tagAll::b -> Tagged All b
            tagAll = Tagged
            Right r1 = poslen_ (Right pat0) (tagAll body0)
-- ^ always succeeds               


instance (R.Regex b, R.RegexLike Re.Regex b) =>
        MatchAll b b (E [[PosLen]]) where
    (~*) pat0 body0 = withRegex' pat0 (tagAll body0) poslen_
      where tagAll::b -> Tagged All b
            tagAll = Tagged



withRegex::(R.Regex a, R.RegexLike Re.Regex b) =>
    a ->        --  pattern 
    hint b ->   --  body
    (Re.Regex -> hint b -> out) -> 
        E out
withRegex p0 b0 fn0 = makeRegex p0 >>= \p1 ->
        Right $ fn0 p1 b0


withRegex'::(R.Regex a, Matchf hint) =>
    a -> hint b ->
        (E Re.Regex -> hint b -> P hint) ->
        P hint
withRegex' p0 b0 fn0 = 
        let er1 = makeRegex p0 
        in fn0 er1 b0  
