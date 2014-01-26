{-# LANGUAGE TupleSections #-}
module Solve
( solve, simpleStep
) where

import Control.Monad (guard)
import Data.Array (assocs, (//), bounds, inRange, (!))
import Data.Set (Set)
import qualified Data.Set as Set

import Base
import Touching
import Check

simpleStep :: Puzzle -> Puzzle
simpleStep z = foldr ($) z
  [ solveUnreachable
  , solveNoPools
  , solveCloseIslands
  , solveRequired
  , solveRiverFlow
  , solveTwoCorner
  , solveStranded
  ]

solve :: Puzzle -> Puzzle
solve z = let
  z' = simpleStep z
  z'' = solveGuess z'
  in if isFull z
    then z
    else if z == z'
      then if z' == z''   -- normal methods did not work.
        then z            -- guess did not work.
        else solve z'' -- guess worked.
      else solve z'    -- normal methods worked.

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
  unknown = Set.fromList [ i | (i, sq) <- assocs z, sq `elem` [Empty, Dot] ]
  otherBorders = Set.unions $ map (border z . blobSpread) otherBlobs
  allowed = Set.union spread $ Set.difference unknown otherBorders
  iteration = size - Set.size spread
  in if iteration < 0
    then spread
    else iterate (`growOnce` allowed) spread !! iteration

solveRequired :: Puzzle -> Puzzle
solveRequired z = z // do
  (blob, others) <- eachWithRest $ getBlobs z
  sq <- Set.toList $ border z $ blobSpread blob
  let domainIfBlack = blobDomain (z // [(sq, Black)]) blob others
  guard $ Set.size domainIfBlack < blobSize blob
  [(sq, Dot)]

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

-- | Solve the common pattern of a 2-island with two ways to expand which are
-- not opposite directions. The square between those two ways is 'Black'.
solveTwoCorner :: Puzzle -> Puzzle
solveTwoCorner z = z // do
  (r, c) <- [ i | (i, Island 2) <- assocs z ]
  horiz <- [-1, 1]
  vert <- [-1, 1]
  let idea = (r + horiz, c + vert)
      expanders = [(r + horiz, c), (r, c + vert)]
      opposites = [(r - horiz, c), (r, c - vert)]
  guard $ all (\p -> safeIndex z p == Just Empty) $ idea : expanders
  guard $ all (\p -> safeIndex z p `notElem` [Just Empty, Just Dot]) opposites
  [(idea, Black)]

-- | Guesses all immediate moves to try to cause a contradiction.
solveGuess :: Puzzle -> Puzzle
solveGuess z = let
  empty = [ p | (p, Empty) <- assocs z ]
  guesses = [ ((sq, Dot  ), z // [(sq, Black)]) | sq <- empty ]
    ++      [ ((sq, Black), z // [(sq, Dot  )]) | sq <- empty ]
  in z // findImpossible guesses

-- | Performs a breadth-first search using 'simpleStep' to travel from layer to
-- layer, until reaching a layer where at least one contradiction is found.
findImpossible :: [(a, Puzzle)] -> [a]
findImpossible []    = []
findImpossible pairs = case [ x | (x, p) <- pairs, checkAll p == Impossible ] of
  [] -> findImpossible $ do
    (x, p) <- pairs
    let p' = simpleStep p
    if p == p'
      then [] -- simpleStep failed, forget about this path
      else [(x, p')]
  imp -> imp

-- | For a blob of 'Dot's with no 'Island' head, if there is only one 'Empty'
-- square bordering it, fills it in with 'Dot'.
solveStranded :: Puzzle -> Puzzle
solveStranded z = z // do
  let claimed = Set.unions $ map blobSpread $ getBlobs z
      dots = Set.fromList [ p | (p, Dot) <- assocs z ]
      unclaimed = Set.difference dots claimed
  p <- Set.toList unclaimed
  let blob = grow (Set.singleton p) dots
  case Set.toList $ border z blob of
    [sq] -> [(sq, Dot)]
    _    -> []
