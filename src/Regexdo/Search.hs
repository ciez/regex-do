{- | wraps functions from stringsearch package

    this module uses newtypes for args plus function names are tweaked

    regex is treated as ordinary String
    -}
module Regexdo.Search
    (break,
    breakFront,
    breakEnd,
    replace,
    split,
    splitEnd,
    splitFront) where
import qualified Data.ByteString.Search as S

import Regexdo.TypeDo hiding (replace)
import Data.ByteString as B hiding (break, breakEnd, split)
import qualified Data.ByteString.Lazy as L
import Prelude hiding (break)

-- ordinary:   a b
-- front:      a \nb
-- end:        a\n b

{- | (front, end)  : drop needle

    >>> break (Needle ":") (Haystack "0123:oid:90")

    ("0123", "oid:90")

    >>> break (Needle "\n") (Haystack "a\nbc\nde")

    ("a", "bc\\nde")     -}
break::Needle ByteString -> Haystack ByteString -> (ByteString, ByteString)
break (Needle pat0) (Haystack b0) =  (h1,t2)
  where (h1,t1) = S.breakOn pat1 b0
        len1 = B.length pat1
        t2 = B.drop len1 t1
        !pat1 = checkPattern pat0


{- | (front, needle + end)

    >>> breakFront (Needle "\n") (Haystack "a\nbc\nde")

    ("a", "\\nbc\\nde")   -}
breakFront::Needle ByteString -> Haystack ByteString -> (ByteString, ByteString)
breakFront (Needle pat0)
  (Haystack b0) = S.breakOn pat1 b0
     where !pat1 = checkPattern pat0

{- | (front + needle, end)

    >>> breakEnd (Needle "\n") (Haystack "a\nbc\nde")

    ("a\\n", "bc\\nde")     -}
breakEnd::Needle ByteString -> Haystack ByteString -> (ByteString, ByteString)
breakEnd (Needle pat0)
  (Haystack b0) = S.breakAfter pat1 b0
     where !pat1 = checkPattern pat0


{- | >>> replace (Needle "\n") (Replacement ",") (Haystack "a\nbc\nde")

    "a,bc,de"       -}
replace::Needle ByteString -> Replacement ByteString -> Haystack ByteString -> ByteString
replace (Needle pat0)
  (Replacement replacement)
  (Haystack b0) = B.concat . L.toChunks $ l
  where l = S.replace pat1 replacement b0
        !pat1 = checkPattern pat0

{- | >>> split (Needle "\n") (Haystack "a\nbc\nde")

    \["a", "bc", "de"]

    >>> split (Needle " ") (Haystack "a bc de")

    \["a", "bc", "de"]

    >>> split (Needle "\\s") (Haystack "abc de fghi ")

    \["abc de fghi "]        -}
split::Needle ByteString -> Haystack ByteString -> [ByteString]
split (Needle pat0)
  (Haystack b0) = S.split pat1 b0
     where !pat1 = checkPattern pat0

{- | >>> splitEnd (Needle "\n") (Haystack "a\nbc\nde")

   \["a\\n", "bc\\n", "de"]      -}
splitEnd::Needle ByteString -> Haystack ByteString -> [ByteString]
splitEnd (Needle pat0)
  (Haystack b0) = S.splitKeepEnd pat1 b0
     where !pat1 = checkPattern pat0

{- | >>> splitFront (Needle "\n") (Haystack "a\nbc\nde")

    \["a", "\\nbc", "\\nde"]     -}
splitFront::Needle ByteString -> Haystack ByteString -> [ByteString]
splitFront (Needle pat0)
  (Haystack b0) = S.splitKeepFront pat1 b0
     where !pat1 = checkPattern pat0

checkPattern::ByteString -> ByteString
checkPattern bs0 = if bs0 == B.empty then error "empty pattern"
      else bs0