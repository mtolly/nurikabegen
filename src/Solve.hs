{-# LANGUAGE TupleSections #-}
module Solve (solve) where

import Control.Monad (guard)
import Data.Array (assocs, (//), bounds, inRange, (!))
import Data.Set (Set)
import qualified Data.Set as Set

import Base

solve :: Puzzle -> Puzzle
solve z = let
  solvers =
    [ solveUnreachable
    , solveNoPools
    , solveCloseIslands
    , solveRequired
    , solveRiverFlow
    ]
  z' = foldr ($) z solvers
  in if isComplete z || z == z'
    then z
    else solve z'

unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f = Set.unions . map f . Set.toList

-- | @growOnce xs ys@ expands @xs@ to include its immediate neighbors, bounded
-- by @ys@.
growOnce :: (Touching a) => Set a -> Set a -> Set a
growOnce xs ys = Set.intersection ys $ unionMap neighbors xs

-- | @grow xs ys@ expands @xs@ to include all reachable elements within @ys@.
grow :: (Touching a) => Set a -> Set a -> Set a
grow xs ys = let xs' = growOnce xs ys in
  if xs == xs' then xs else grow xs' ys

safeIndex :: Puzzle -> Posn -> Maybe Square
safeIndex z p = guard (inRange (bounds z) p) >> Just (z ! p)

-- | Returns 'True' if filling in the square with 'Black' would create a pool.
wouldPool :: Puzzle -> Posn -> Bool
wouldPool z (r, c) = let
  pools = do
    dr <- [-1, 1]
    dc <- [-1, 1]
    return [(r + dr, c + dc), (r + dr, c), (r, c + dc)]
    :: [[Posn]]
  allBlack = all (== Just Black) . map (safeIndex z)
  in any allBlack pools

-- | Fills 'Empty' squares with 'Dot' which would create pools if 'Black'.
solveNoPools :: Puzzle -> Puzzle
solveNoPools z = z // [ (i, Dot) | (i, Empty) <- assocs z, wouldPool z i ]

-- | Representation of a (complete or incomplete) island and its squares.
data Blob = Blob
  { blobSpread :: Set Posn
  , blobSize   :: Int
  } deriving (Eq, Ord, Show, Read)

-- | Finds all the islands and the squares they currently own.
getBlobs :: Puzzle -> [Blob]
getBlobs z = let
  islandHeads = [ (i, n) | (i, Island n) <- assocs z ]
  allLand = Set.fromList [ i | (i, sq) <- assocs z, notElem sq [Black, Empty] ]
  in flip map islandHeads $ \(i, n) -> Blob
    { blobSpread = grow (Set.singleton i) allLand
    , blobSize   = n
    }

-- | Returns all 'Empty' squares next to the given 'blobSpread'.
border :: Puzzle -> Set Posn -> Set Posn
border z s = Set.filter (\n -> safeIndex z n == Just Empty) $
  unionMap neighbors s

-- | Fills in 'Empty' squares with 'Black' which are next to finished islands.
solveCloseIslands :: Puzzle -> Puzzle
solveCloseIslands z = z // do
  Blob spread size <- getBlobs z
  guard $ Set.size spread == size
  map (, Black) $ Set.toList $ border z spread

eachWithRest :: [a] -> [(a, [a])]
eachWithRest []       = []
eachWithRest (x : xs) = (x, xs) : [ (y, x : ys) | (y, ys) <- eachWithRest xs ]

getUnreachable :: Puzzle -> Set Posn
getUnreachable z = let
  unknown = Set.fromList [ i | (i, Empty) <- assocs z ]
  blobDomains = map (uncurry $ blobDomain z) $ eachWithRest $ getBlobs z
  in Set.difference unknown $ Set.unions blobDomains

-- | Fills in 'Empty' squares with 'Black' if they can't possibly be reached
-- by any existing island.
solveUnreachable :: Puzzle -> Puzzle
solveUnreachable z = z // map (, Black) (Set.toList $ getUnreachable z)

-- | Given a blob and all other blobs, finds the possible squares the blob could
-- extend to.
blobDomain :: Puzzle -> Blob -> [Blob] -> Set Posn
blobDomain z (Blob spread size) otherBlobs = let
  unknown = Set.fromList [ i | (i, Empty) <- assocs z ]
  otherBorders = Set.unions $ map (border z . blobSpread) otherBlobs
  allowed = Set.union spread $ Set.difference unknown otherBorders
  in iterate (`growOnce` allowed) spread !! (size - Set.size spread)

solveRequired :: Puzzle -> Puzzle
solveRequired z = let
  blobs = eachWithRest $ getBlobs z
  required :: Set Posn
  required = Set.unions $ flip map blobs $ \(blob, others) -> let
    checkSq sq = let
      dom = blobDomain (z // [(sq, Black)]) blob others
      in Set.size dom < blobSize blob
    in Set.filter checkSq $ border z $ blobSpread blob
  in z // map (, Dot) (Set.toList required)

-- | Fills in squares with 'Black' which, if filled with 'Dot', would prevent
-- all the remaining 'Black' squares from being able to connect.
solveRiverFlow :: Puzzle -> Puzzle
solveRiverFlow z = let
  blacks = Set.fromList [ i | (i, Black) <- assocs z ]
  unknown = Set.fromList [ i | (i, Empty) <- assocs z ]
  wouldObstruct sq = not $ blacks `allConnectedVia` Set.delete sq unknown
  in z // map (, Black) (Set.toList $ Set.filter wouldObstruct unknown)

-- | @xs `allConnectedVia` ys@ checks if xs can all be connected to each other
-- using any square in (xs `union` ys) as a bridge.
allConnectedVia :: (Touching a) => Set a -> Set a -> Bool
allConnectedVia xs ys = case Set.minView xs of
  Nothing     -> True
  Just (h, _) -> let
    zs = grow (Set.singleton h) (Set.union xs ys)
    in Set.null $ Set.difference xs zs
