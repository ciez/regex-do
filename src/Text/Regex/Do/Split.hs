{- | see "Data.ByteString.Search" package

    regex is treated as ordinary String
    -}
module Text.Regex.Do.Split
    (break,
    replace,
    split,
    KeepNeedle(..)) where

import qualified Data.ByteString.Search as S
import Text.Regex.Do.Type.Do hiding (replace)
import Data.ByteString as B hiding (break, breakEnd, split)
import qualified Data.ByteString.Lazy as L
import Prelude hiding (break)


data KeepNeedle = Drop  -- ^ needle between parts disappears
        | Front -- ^ needle sticks to front of next part
        | End   -- ^ needle sticks to end of previous part

{- | >>> replace (Pattern "\n") (Replacement ",") (Body "a\nbc\nde")

    "a,bc,de"       -}
replace::Pattern ByteString -> Replacement ByteString -> Body ByteString -> ByteString
replace (Pattern pat0)
  (Replacement replacement)
  (Body b0) = B.concat . L.toChunks $ l
  where l = S.replace pat1 replacement b0
        !pat1 = checkPattern pat0



-- ordinary:   a b
-- front:      a \nb
-- end:        a\n b

{- | >>> break Drop (Pattern "\n") (Body "a\nbc\nde")

    ("a", "bc\\nde")


    >>> break Front (Pattern "\n") (Body "a\nbc\nde")

    ("a", "\\nbc\\nde")


    >>> break End (Pattern "\n") (Body "a\nbc\nde")

    ("a\\n", "bc\\nde")     -}

break::KeepNeedle ->
        Pattern ByteString -> Body ByteString ->
            (ByteString, ByteString)
break case0 = case case0 of
                Drop -> break'
                Front -> breakFront
                End -> breakEnd


break'::Pattern ByteString -> Body ByteString -> (ByteString, ByteString)
break' (Pattern pat0) (Body b0) =  (h1,t2)
  where (h1,t1) = S.breakOn pat1 b0
        len1 = B.length pat1
        t2 = B.drop len1 t1
        !pat1 = checkPattern pat0


breakFront::Pattern ByteString -> Body ByteString -> (ByteString, ByteString)
breakFront (Pattern pat0)
  (Body b0) = S.breakOn pat1 b0
     where !pat1 = checkPattern pat0


breakEnd::Pattern ByteString -> Body ByteString -> (ByteString, ByteString)
breakEnd (Pattern pat0)
  (Body b0) = S.breakAfter pat1 b0
     where !pat1 = checkPattern pat0




{- | >>> split Drop (Pattern " ") (Body "a bc de")

    \["a", "bc", "de"]

    /space may be used/

   >>> split Front (Pattern "\n") (Body "a\nbc\nde")

    \["a", "\\nbc", "\\nde"]


    >>> split End (Pattern "\n") (Body "a\nbc\nde")

   \["a\\n", "bc\\n", "de"]     -}

split::KeepNeedle ->
    Pattern ByteString -> Body ByteString ->
        [ByteString]
split case0 = case case0 of
                Drop -> split'
                Front -> splitFront
                End -> splitEnd


split'::Pattern ByteString -> Body ByteString -> [ByteString]
split' (Pattern pat0)
  (Body b0) = S.split pat1 b0
     where !pat1 = checkPattern pat0

splitEnd::Pattern ByteString -> Body ByteString -> [ByteString]
splitEnd (Pattern pat0)
  (Body b0) = S.splitKeepEnd pat1 b0
     where !pat1 = checkPattern pat0

splitFront::Pattern ByteString -> Body ByteString -> [ByteString]
splitFront (Pattern pat0)
  (Body b0) = S.splitKeepFront pat1 b0
     where !pat1 = checkPattern pat0


checkPattern::ByteString -> ByteString
checkPattern bs0 = if bs0 == B.empty then error "empty pattern"
      else bs0