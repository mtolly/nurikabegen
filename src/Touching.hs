{-# LANGUAGE TupleSections #-}
module Touching where

import Data.Set (Set)
import qualified Data.Set as Set

class (Ord a) => Touching a where
  touches :: a -> a -> Bool
  neighbors :: a -> Set a
  x `touches` y = Set.member x $ neighbors y

instance Touching Int where
  x `touches` y = abs (x - y) <= 1
  neighbors x = Set.fromList [x, x + 1, x - 1]

instance (Touching a, Touching b) => Touching (a, b) where
  neighbors (x, y) = Set.union
    (Set.map (, y) $ neighbors x)
    (Set.map (x ,) $ neighbors y)
  (a, b) `touches` (c, d) = or
    [ and [a == c, b `touches` d]
    , and [b == d, a `touches` c]
    ]

unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f = Set.unions . map f . Set.toList

-- | @growOnce xs ys@ expands @xs@ to include its immediate neighbors, bounded
-- by @ys@.
growOnce :: (Touching a) => Set a -> Set a -> Set a
growOnce xs ys = Set.intersection ys $ unionMap neighbors xs

-- | @grow xs ys@ expands @xs@ to include all reachable elements within @ys@.
grow :: (Touching a) => Set a -> Set a -> Set a
grow xs ys = let xs' = growOnce xs ys in
  if Set.size xs == Set.size xs' then xs else grow xs' ys
