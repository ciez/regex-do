{- | see "Data.ByteString.Search" package

    this module uses newtypes for args

    regex is treated as ordinary String
    -}
module Text.Regex.Do.Split
    (break,
    breakFront,
    breakEnd,
    replace,
    split,
    splitEnd,
    splitFront) where

import qualified Data.ByteString.Search as S
import Text.Regex.Do.TypeDo hiding (replace)
import Data.ByteString as B hiding (break, breakEnd, split)
import qualified Data.ByteString.Lazy as L
import Prelude hiding (break)

-- ordinary:   a b
-- front:      a \nb
-- end:        a\n b

{- | (front, end)  : drop needle

    >>> break (Pattern ":") (Body "0123:oid:90")

    ("0123", "oid:90")

    >>> break (Pattern "\n") (Body "a\nbc\nde")

    ("a", "bc\\nde")     -}
break::Pattern ByteString -> Body ByteString -> (ByteString, ByteString)
break (Pattern pat0) (Body b0) =  (h1,t2)
  where (h1,t1) = S.breakOn pat1 b0
        len1 = B.length pat1
        t2 = B.drop len1 t1
        !pat1 = checkPattern pat0


{- | (front, needle + end)

    >>> breakFront (Pattern "\n") (Body "a\nbc\nde")

    ("a", "\\nbc\\nde")   -}
breakFront::Pattern ByteString -> Body ByteString -> (ByteString, ByteString)
breakFront (Pattern pat0)
  (Body b0) = S.breakOn pat1 b0
     where !pat1 = checkPattern pat0

{- | (front + needle, end)

    >>> breakEnd (Pattern "\n") (Body "a\nbc\nde")

    ("a\\n", "bc\\nde")     -}
breakEnd::Pattern ByteString -> Body ByteString -> (ByteString, ByteString)
breakEnd (Pattern pat0)
  (Body b0) = S.breakAfter pat1 b0
     where !pat1 = checkPattern pat0


{- | >>> replace (Pattern "\n") (Replacement ",") (Body "a\nbc\nde")

    "a,bc,de"       -}
replace::Pattern ByteString -> Replacement ByteString -> Body ByteString -> ByteString
replace (Pattern pat0)
  (Replacement replacement)
  (Body b0) = B.concat . L.toChunks $ l
  where l = S.replace pat1 replacement b0
        !pat1 = checkPattern pat0

{- | >>> split (Pattern "\n") (Body "a\nbc\nde")

    \["a", "bc", "de"]

    >>> split (Pattern " ") (Body "a bc de")

    \["a", "bc", "de"]

    >>> split (Pattern "\\s") (Body "abc de fghi ")

    \["abc de fghi "]        -}
split::Pattern ByteString -> Body ByteString -> [ByteString]
split (Pattern pat0)
  (Body b0) = S.split pat1 b0
     where !pat1 = checkPattern pat0

{- | >>> splitEnd (Pattern "\n") (Body "a\nbc\nde")

   \["a\\n", "bc\\n", "de"]      -}
splitEnd::Pattern ByteString -> Body ByteString -> [ByteString]
splitEnd (Pattern pat0)
  (Body b0) = S.splitKeepEnd pat1 b0
     where !pat1 = checkPattern pat0

{- | >>> splitFront (Pattern "\n") (Body "a\nbc\nde")

    \["a", "\\nbc", "\\nde"]     -}
splitFront::Pattern ByteString -> Body ByteString -> [ByteString]
splitFront (Pattern pat0)
  (Body b0) = S.splitKeepFront pat1 b0
     where !pat1 = checkPattern pat0

checkPattern::ByteString -> ByteString
checkPattern bs0 = if bs0 == B.empty then error "empty pattern"
      else bs0