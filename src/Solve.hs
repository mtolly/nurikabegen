{-# LANGUAGE TupleSections #-}
module Solve
( solve, solve'
) where

import Control.Monad (guard)
import Data.Array (assocs, (//), bounds, inRange, (!))
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Base
import Touching
import Check

solve :: Puzzle -> Puzzle
solve = solve' 1

-- | The 'Int' is the allowed depth of guessing.
solve' :: Int -> Puzzle -> Puzzle
solve' g z = let
  solvers =
    [ solveUnreachable
    , solveNoPools
    , solveCloseIslands
    , solveRequired
    , solveRiverFlow
    , solveTwoCorner
    , solveStranded
    ]
  z' = foldr ($) z solvers
  z'' = solveGuess (g - 1) z'
  in if isFull z
    then z
    else if z == z'
      then if g > 0         -- normal methods did not work.
        then if z' == z''   -- let's try guessing.
          then z            -- guess did not work.
          else solve' g z'' -- guess worked! recurse anew.
        else z              -- not allowed to guess.
      else solve z'         -- normal methods worked.

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

-- | Guesses all immediate moves to try to cause a contradiction. The 'Int' is
-- the allowed depth of subguesses.
solveGuess :: Int -> Puzzle -> Puzzle
solveGuess g z = let
  empty = [ p | (p, Empty) <- assocs z ]
  guess p = case checkAll $ solve' g $ z // [(p, Dot)] of
    Impossible -> Just $ z // [(p, Black)]
    _          -> case checkAll $ solve' g $ z // [(p, Black)] of
      Impossible -> Just $ z // [(p, Dot)]
      _          -> Nothing
  in case mapMaybe guess empty of
    z' : _ -> z'
    _      -> z

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
