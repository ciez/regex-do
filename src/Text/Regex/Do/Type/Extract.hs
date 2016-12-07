module Text.Regex.Do.Type.Extract where

import Text.Regex.Base.RegexLike as R hiding (empty)
import qualified Text.Regex.Base.RegexLike as R (empty)
import Prelude as P
import Data.ByteString as B
import Data.Text as T hiding (empty)
import qualified Data.Text as T (empty)
import Text.Regex.Do.Type.Do


{- | see String, ByteString instances for implementation examples

    see "Text.Regex.Base.RegexLike" for 'Extract' detail        -}
class Extract a => Extract' a where
   concat'::[a] -> a
   len'::a -> Int


instance Extract' String where
   concat' = P.concat
   len' = P.length


instance Extract' B.ByteString where
   concat' = B.concat
   len' = B.length


instance Extract Text where
    before = T.take
    after = T.drop
    empty = T.empty


instance Extract' Text where
    concat' = T.concat
    len' = T.length


prefix::Extract a => PosLen -> a -> a
prefix pl0 = before $ fst pl0

suffix::Extract a => PosLen -> a -> a
suffix pl0 = after (pos1 + len1)
  where pos1 = fst pl0
        len1 = snd pl0