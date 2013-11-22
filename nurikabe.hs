{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Array
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Control.Monad (guard)

import qualified Data.Set as Set
import Data.Set (Set)

--import Debug.Trace (trace)
--import System.Exit

main :: IO ()
main = putStrLn $ showPuzzle $ solve puzzle2

isComplete :: Puzzle -> Bool
isComplete z = null [ i | (i, Empty) <- assocs z ]

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

-- | Square of a (complete or incomplete) Nurikabe puzzle.
data Square
  = Empty      -- ^ Unknown square
  | Dot        -- ^ Known to be part of an island
  | Black      -- ^ Known to be river
  | Island Int -- ^ The head of an island
  deriving (Eq, Ord, Show, Read)

type Posn = (Int, Int)
type Puzzle = Array Posn Square

-- | Used for checking various game rules.
data Status = Possible | Done | Impossible
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Monoid Status where
  mempty = Done
  mappend Impossible _          = Impossible
  mappend _          Impossible = Impossible
  mappend Done       Done       = Done
  mappend _          _          = Possible

puzzle1 :: Puzzle
puzzle1 = readPuzzle $ unlines
  [ "----2-2---"
  , "2-------2-"
  , "---2------"
  , "-2----2---"
  , "-----2-2--"
  , "--2-------"
  , "-----2-2--"
  , "--2------2"
  , "2----2----"
  , "--------2-"
  ]

puzzle2 :: Puzzle
puzzle2 = readPuzzle $ unlines
  [ "--3----2--"
  , "---3-----2"
  , "--2-------"
  , "---3-----3"
  , "-3--------"
  , "----3----3"
  , "--2---2---"
  , "-----3----"
  , "----------"
  , "-3-----2-2"
  ]

readPuzzle :: String -> Puzzle
readPuzzle s = let
  squares = filter (not . null) $ map (mapMaybe readSquare) $ lines s
  height = length squares
  width = maximum $ map length squares
  in listArray ((0, 0), (height - 1, width - 1)) $ concat squares

readSquare :: Char -> Maybe Square
readSquare c = lookup (toUpper c) $ concat
  [ [('_', Empty), ('-', Empty), ('.', Dot), ('#', Black)]
  , zip ['0'..'9'] $ map Island [0..]
  , zip ['A'..'Z'] $ map Island [10..]
  ]

showSquare :: Square -> Char
showSquare s = case s of
  Empty    -> '_'
  Dot      -> '.'
  Black    -> '#'
  Island i -> (['0'..'9'] ++ ['A'..'Z']) !! i

showPuzzle :: Puzzle -> String
showPuzzle z = let
  ((_, c0), (_, c1)) = bounds z
  width = c1 - c0 + 1
  groupsOf _ [] = []
  groupsOf i xs = let
    (a, b) = splitAt i xs
    in a : groupsOf i b
  in unlines $ groupsOf width $ map showSquare $ elems z

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
