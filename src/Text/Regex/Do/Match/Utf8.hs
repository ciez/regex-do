{- | see "Text.Regex.Base.RegexLike" 

    see "Text.Regex.Do.Pcre.Match.Latin" for API changes

    __'toByteString'__ converts String to utf8 'ByteString'     -}
module Text.Regex.Do.Match.Utf8
    (MatchOnce(..),
    MatchAll(..),
    R.extract   -- | 'extract' is reexport from "Text.Regex.Base.RegexLike"
    ) where

import Data.Tagged
import qualified Text.Regex.Base.RegexLike as R hiding (makeRegex)
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Match.Matchf as F
import Text.Regex.PCRE.Wrap()
import Text.Regex.Do.Match.Regex as T
import Text.Regex.Do.Type.MatchHint
import Text.Regex.Do.Match.Option
import Text.Regex.Do.Type.Reexport as Re
import Data.ByteString
import Text.Regex.Do.Type.Convert


{- | * pattern:  'String', 'ByteString', 'Regex'      compile 'Re.Regex' with 'Utf8' opt

        String | ByteString pattern may contains regex
    
    * body:  'String', 'ByteString'
    * out: out | E out

        * 'Bool'  :  test if regex matches
        * ['String'] 
        * ['ByteString']
        * ['PosLen']    -}
class MatchOnce pattern body out where
    (~?)::pattern -> body -> out


{- | * pattern:  'String', 'ByteString', 'Re.Regex'      compile 'Re.Regex' with 'Utf8' opt
    * body:  'String', 'ByteString'
    * out:

        * [['String']]
        * [['ByteString']]
        * [['PosLen']]      -}
class MatchAll pattern body out where
    (~*)::pattern -> body -> out


{- | b: 'String', 'ByteString' 

    always succeeds               

==== precompiled regex as pattern

@    
let rx1 = makeRegexOpt (toByteString "左") [Utf8] []      --  add options as needed
    m1 = rx1 -? (toByteString "100メートル左折後、左")::[ByteString]
m1 `shouldBe` [toByteString "左"]       
@ -}

instance R.RegexLike Re.Regex b => 
        MatchOnce Re.Regex b [b] where
    (~?) pat0 body0 = once pat0 (Tagged body0)


instance R.RegexLike Re.Regex b => 
        MatchOnce Re.Regex b Bool where
    (~?) pat0 body0 = test pat0 (Tagged body0)
{- ^ b: 'String', 'ByteString' 

always succeeds     -}


instance R.RegexLike Re.Regex b =>
        MatchOnce Re.Regex b [PosLen] where
    (~?) pat0 body0 = r1
        where tagOne::b -> Tagged Once b
              tagOne = Tagged 
              Right r1 = poslen_ (Right pat0) (tagOne body0)
{- ^ b: 'String', 'ByteString' 

always succeeds     -}


instance MatchOnce String String (E [String]) where
    (~?) pat0 body0 = withRegex pat0 (tagB body0) once
        where tagB::b -> Tagged Once b
              tagB = Tagged
{- ^ >>> ("^熱"::String) ~? ("熱い午後"::String)::E [String]

Right \["熱"]     -}


instance MatchOnce ByteString ByteString (E [ByteString]) where
    (~?) pat0 body0 = withRegex pat0 (tagB body0) once
        where tagB::b -> Tagged Once b
              tagB = Tagged


instance MatchOnce String String (E Bool) where
    (~?) pat0 body0 = withRegex pat0 (tagB body0) test
        where tagB::b -> Tagged Test b
              tagB = Tagged
{- ^ test

>>>  ("в"::String) ~? ("тихо в лесу"::String)::E Bool

Right True        -}

instance MatchOnce ByteString ByteString (E Bool) where
    (~?) pat0 body0 = withRegex pat0 (tagB body0) test
        where tagB::b -> Tagged Test b
              tagB = Tagged


instance MatchOnce ByteString ByteString (E [PosLen]) where
    (~?) pat0 body0 = withRegex' pat0 (tagOne body0) poslen_
      where tagOne::b -> Tagged Once b
            tagOne = Tagged


instance MatchOnce String String (E [PosLen]) where
    (~?) pat0 body0 = withRegex' pat0 (tagOne body0) poslen_
      where tagOne::b -> Tagged Once b
            tagOne = Tagged


instance R.RegexLike Re.Regex b =>
        MatchAll Re.Regex b [[b]] where
    (~*) pat0 body0 = F.all pat0 (Tagged body0)
{- ^ b: 'String', 'ByteString' 

always succeeds     -}



instance MatchAll ByteString ByteString (E [[ByteString]]) where
    (~*) pat0 body0 = withRegex pat0 (Tagged body0) F.all


instance MatchAll String String (E [[String]]) where
    (~*) pat0 body0 = withRegex pat0 (Tagged body0) F.all
{- ^  >>> ("лес"::String) ~* ("Залесью, залесью…"::String)::E [[String]]

Right \[["лес"],["лес"]]        -}


instance R.RegexLike Re.Regex b =>
        MatchAll Re.Regex b [[PosLen]] where
    (~*) pat0 body0 = r1
        where tagOne::b -> Tagged All b
              tagOne = Tagged
              Right r1 = poslen_ (Right pat0) (tagOne body0)
{- ^ b: 'String', 'ByteString' 

always succeeds     -}



instance MatchAll String String (E [[PosLen]]) where
    (~*) pat0 body0 = withRegex' pat0 (tagAll body0) poslen_
      where tagAll::b -> Tagged All b
            tagAll = Tagged


instance MatchAll ByteString ByteString (E [[PosLen]]) where
    (~*) pat0 body0 = withRegex' pat0 (tagAll body0) poslen_
      where tagAll::b -> Tagged All b
            tagAll = Tagged



class WithRegex a out' out where
    withRegex::(T.Regex a, R.RegexLike Re.Regex a, Functor hint) =>
        a ->        --  pattern 
        hint a ->   --  body
        (Re.Regex -> hint ByteString -> out') ->
            E out


instance WithRegex ByteString out out where
    withRegex p0 b0 fn0 = makeRegexOpt p0 [Utf8] [] >>= \p1 ->
            Right $ fn0 p1 b0


instance WithRegex String [ByteString] [String] where
    withRegex p0 b0 fn0 = makeRegexOpt p1 [Utf8] [] >>= \p2 ->
            Right $ toString <$> (fn0 p2 b1)
        where p1 = toByteString p0
              b1 = toByteString <$> b0


instance WithRegex String [[ByteString]] [[String]] where
    withRegex p0 b0 fn0 = makeRegexOpt p1 [Utf8] [] >>= \p2 ->
            Right $ [(toString <$>)] <*> (fn0 p2 b1)
        where p1 = toByteString p0
              b1 = toByteString <$> b0


instance WithRegex String Bool Bool where
    withRegex p0 b0 fn0 = makeRegexOpt p1 [Utf8] [] >>= \p2 ->
            Right $ fn0 p2 b1
        where p1 = toByteString p0
              b1 = toByteString <$> b0


withRegex'::(T.Regex a, Matchf hint) =>
    a -> hint b ->
        (E Re.Regex -> hint b -> P hint) ->
        P hint
withRegex' p0 b0 fn0 =
        let er1 = makeRegexOpt p0 [Utf8] []
        in fn0 er1 b0  
