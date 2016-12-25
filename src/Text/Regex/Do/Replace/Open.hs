{- | extensible and reusable replacement functions

Run replacement with your preferred content types e.g. "Data.Text" (implemented),

from search results with non-PCRE regex or non-regex libs

=== how to use:    

value replacement:

>>> replace (Just [(4,3)::PosLen]) "4567" ("abc 123 def"::Text)

"abc 4567 def"


'GroupReplacer' : replace with a function

@
replacer::GroupReplacer Text
replacer = defaultReplacer 1 tweak1        --  1: group 1 match. 
          where tweak1 str1 = case str1 of
                                "123" -> "[1-2-3]"
                                otherwise -> traceShow str1 "?"
@

>>> replace (Just ([(4,3)]::[PosLen])) replacer ("abc 123 def"::Text)

    "abc [1-2-3] def"     -}    

module Text.Regex.Do.Replace.Open
    (Replace(..),
    defaultReplacer,
    getGroup,
    replaceMatch,
    boundsOk)
    where

import Text.Regex.Base.RegexLike as R
import Data.Array as A
import Prelude as P
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Match.Result as R
import Text.Regex.Do.Type.Convert
import Text.Regex.Do.Type.Extract


class Replace f repl body where
   replace::(Extract' body, ToArray arr) =>
        f arr -> repl -> body -> body


instance Replace Maybe b b where
   replace Nothing repl0 body0 = body0
   replace (Just ma0) repl0 body0 = firstGroup lpl1 (repl0, body0)
        where lpl1 = A.elems $ toArray ma0


instance Replace [] b b where
   replace [] _ body0 = body0
   replace ma0 repl0 body0 =
      let lpl1 = R.poslen $ toArray <$> ma0::[[PosLen]]
          foldFn1 lpl1 acc1 = firstGroup lpl1 (repl0,acc1)
      in P.foldr foldFn1 body0 lpl1


instance Replace Maybe (GroupReplacer b) b where
   replace Nothing _ body0 = body0
   replace (Just ma0) (GroupReplacer repl0) body0 =
            let a1 = ReplaceAcc {
                                  acc = body0,
                                  pos_adj = 0
                                }
            in acc $ repl0 (toArray ma0) a1


instance Replace [] (GroupReplacer b) b where
   replace [] _ body0 = body0
   replace ma0 (GroupReplacer repl0) body0 =
        let acc1 = ReplaceAcc { acc = body0, pos_adj = 0 }
        in acc $ P.foldl (flip repl0) acc1 $ toArray <$> ma0


firstGroup::Extract' a =>
    [PosLen] -> (a,a) -> a
firstGroup (pl0:_) r1@(new0,a0) = acc $ replaceMatch pl0 (new0, acc1)
    where acc1 = ReplaceAcc {
                    acc = a0,
                    pos_adj = 0
                    }


--  dynamic
{- | Replaces specified (by idx) group match with value provided by (a -> a) fn.
    Works for one common simple use case

    'GroupReplacer' can also be used with multi-group regex

    another custom dynamic replacer could e.g.
    inspect all group matches before looking up a replacement.     -}
defaultReplacer::Extract' a =>
        Int         -- ^ group idx. 0: full match, groups: 1.. see 'MatchArray'
        -> (a -> a) -- ^ (group match -> replacement) lookup
            -> GroupReplacer a
defaultReplacer idx0 tweak0 = GroupReplacer fn1
    where fn1 (ma0::MatchArray) acc0 =
            if boundsOk ma0 idx0 then maybe acc0 fn1 mval1
            else acc0
            where pl1 = ma0 A.! idx0 :: (R.MatchOffset, R.MatchLength)
                  mval1 = getGroup acc0 ma0 idx0 
                  fn1 str1 = replaceMatch pl1 (str2, acc0) 
                             where str2 = tweak0 str1


{- | check if specified group index is within 'MatchArray' bounds

for use within 'GroupReplacer'
-}
boundsOk::MatchArray -> Int -> Bool
boundsOk ma0 = inRange (bounds ma0)                         


{- | get group content safely:

    * non-existing group idx will not error but return 'Nothing'
    * adjust for previous replacements length

    see 'defaultReplacer' source for use example
    -}
getGroup::R.Extract a =>
    ReplaceAcc a -> MatchArray -> Int -> Maybe a
getGroup acc0 ma0 idx0 = if not (boundsOk ma0 idx0) then Nothing     --  safety catch
    else Just val1
    where pl1 = ma0 A.! idx0 :: (R.MatchOffset, R.MatchLength)
          pl2 = adjustPoslen pl1 acc0
          val1 = extract pl2 $ acc acc0


{- | replace group match while adjusting for previous replacements length

    see 'defaultReplacer' source for use example     -}

replaceMatch::Extract' a =>
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


adjustPoslen::PosLen -> ReplaceAcc a -> PosLen
adjustPoslen (p0,l0) acc0  = (p0 + pos_adj acc0, l0)
