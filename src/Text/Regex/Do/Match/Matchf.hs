-- | __internal__ module, exposed only to show the instances
module Text.Regex.Do.Match.Matchf where

import Text.Regex.Do.Type.Do
import Text.Regex.Do.Type.Reexport as R
import Text.Regex.Do.Match.Result as R
import Text.Regex.Base.RegexLike as R
import Text.Regex.Do.Type.MatchHint
import Data.Tagged
import Data.ByteString as B 


type R_ b = R.RegexLike R.Regex b


class Matchf hint where
    type H hint
    type P hint
    marray_::R_ b => E R.Regex -> hint b -> H hint
    poslen_::R_ b => E R.Regex -> hint b -> P hint
    
    
instance Matchf (Tagged Once) where
    type H (Tagged Once) = E (Maybe MatchArray)
    type P (Tagged Once) = E [PosLen]
    marray_ ep0 (Tagged b0) = ep0 >>= \ep1 -> Right $ R.matchOnce ep1 b0 
    poslen_ ep0 (Tagged b0) = ep0 >>= \ep1 -> 
                let mpl1 = R.poslen $ R.matchOnce ep1 b0::Maybe [PosLen]
                in Right $ maybe [] id mpl1 


instance Matchf (Tagged All) where
    type H (Tagged All) = E [MatchArray]
    type P (Tagged All) = E [[PosLen]]
    marray_ ep0 (Tagged b0) = ep0 >>= \ep1 -> Right $ R.matchAll ep1 b0
    poslen_ ep0 (Tagged b0) = ep0 >>= \ep1 -> Right $ R.poslen $ R.matchAll ep1 b0


once::(R_ b, R.Extract b) =>
    R.Regex -> Tagged Once b -> [b]      -- ^ matched content
once p0 t0@(Tagged b0) = maybe [] id $ R.allMatches (Body b0) r1 
    where Right r1 = marray_ (Right p0) t0   


all::(R_ b, R.Extract b) =>
    R.Regex -> Tagged All b -> [[b]]       -- ^ matched content
all p0 t0@(Tagged b0) = R.allMatches (Body b0) r1 
    where Right r1 = marray_ (Right p0) t0


test::(R_ b, R.Extract b) =>
    R.Regex -> Tagged Test b -> Bool      -- ^ matched content
test p0 t0@(Tagged b0) = R.matchTest p0 b0



tagOnce::b -> Tagged Once b
tagOnce = Tagged

tagAll::b -> Tagged All b
tagAll = Tagged


checkPattern::ByteString -> ByteString
checkPattern bs0 = if bs0 == B.empty then error "empty pattern"
      else bs0