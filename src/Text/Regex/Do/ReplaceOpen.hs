{- | extensible and reusable replacement functions

    Run replacement with your preferred content types e.g. "Data.Text",
    from search results with non-PCRE regex or non-regex libs

    open an issue or a PR on <https://github.com/ciez/regex-do git> to request a new 'Extract'' instance

    "Data.Text" instance already works  -}

module Text.Regex.Do.ReplaceOpen
    (ReplaceOpen(..),
    defaultReplacer,
    getGroup,
    replaceMatch
    )
    where

import Text.Regex.Base.RegexLike as R
import Data.Array as A
import Prelude as P
import Text.Regex.Do.Type.Do
import Text.Regex.Do.Result as R
import Text.Regex.Do.Convert
import Text.Regex.Do.Type.Extract


{- | 'Replacement':

    >>> replace (Just [(4,3)::PosLen]) (Replacement "4567") (Body "abc 123 def"::Body Text)

    "abc 4567 def"


    'GroupReplacer' :

    >>> replacer::GroupReplacer Text
        replacer = defaultReplacer 1 tweak1        --  1: first match in group
              where tweak1 str1 = case str1 of
                                    "123" -> "[1-2-3]"
                                    otherwise -> traceShow str1 "?"

    >>> replace (Just ([(4,3),(8,2)]::[PosLen])) replacer (Body "abc 123 def"::Body Text)

        "abc [1-2-3] def"     -}

class ReplaceOpen f r where
   replace::(Extract' a, ToArray arr) =>
        f arr -> r a -> Body a -> a


instance ReplaceOpen Maybe Replacement where
   replace Nothing (Replacement repl0) (Body b0) = b0
   replace (Just ma0) (Replacement repl0) (Body b0) = firstGroup lpl1 (repl0, b0)
        where lpl1 = A.elems $ toArray ma0


instance ReplaceOpen [] Replacement where
   replace [] _ (Body b0) = b0
   replace ma0 (Replacement repl0) (Body b0) =
      let lpl1 = R.poslen $ toArray <$> ma0::[[PosLen]]
          foldFn1 lpl1 acc1 = firstGroup lpl1 (repl0,acc1)
      in P.foldr foldFn1 b0 lpl1


instance ReplaceOpen Maybe GroupReplacer where
   replace Nothing _ (Body b0) = b0
   replace (Just ma0) (GroupReplacer repl0) (Body b0) =
            let a1 = ReplaceAcc {
                                  acc = b0,
                                  pos_adj = 0
                                }
            in acc $ repl0 (toArray ma0) a1


instance ReplaceOpen [] GroupReplacer where
   replace [] _ (Body b0) = b0
   replace ma0 (GroupReplacer repl0) (Body b0) =
        let acc1 = ReplaceAcc { acc = b0, pos_adj = 0 }
        in acc $ P.foldl (flip repl0) acc1 $ toArray <$> ma0


firstGroup::Extract' a =>
    [PosLen] -> (a,a) -> a
firstGroup (pl0:_) r1@(new0,a0) = acc $ replaceMatch pl0 (new0, acc1)
    where acc1 = ReplaceAcc {
                    acc = a0,
                    pos_adj = 0
                    }


--  dynamic
{- | Replaces specified (by idx) group match with tweaked value.
    Works for one common simple use case

    'GroupReplacer' can be used with complicated regex

    another custom dynamic replacer could e.g.
    inspect all group matches before looking up a replacement.     -}
defaultReplacer::Extract' a =>
        Int         -- ^ group idx
        -> (a -> a) -- ^ (group match -> replacement) tweak
            -> GroupReplacer a
defaultReplacer idx0 tweak0 = GroupReplacer fn1
    where fn1 (ma0::MatchArray) acc0 = maybe acc0 fn1 mval1
                where pl1 = ma0 A.! idx0 :: (R.MatchOffset, R.MatchLength)
                      mval1 = getGroup acc0 ma0 idx0
                      fn1 str1 = replaceMatch pl1 (str2, acc0)
                                 where str2 = tweak0 str1


{- | get group content safely:

    * non-existing group idx will not error but return 'Nothing'
    * adjust for previous replacements length

    see 'defaultReplacer' source for use example
    -}
getGroup::R.Extract a =>
    ReplaceAcc a -> MatchArray -> Int -> Maybe a
getGroup acc0 ma0 idx0 = if idx0 >= P.length ma0 then Nothing     --  safety catch
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
