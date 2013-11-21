{-# LANGUAGE BangPatterns, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Array
import Data.Char (toUpper)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid
import Control.Monad (guard)

import qualified Data.Set as Set
import Data.Set (Set)

--import Debug.Trace (trace)
--import System.Exit

main :: IO ()
main = putStrLn $ showPuzzle $
  solveCloseIslands $ solveNoPools $ solveCloseIslands $
  solveNoPools $ solveCloseIslands $ solveNoPools $
  solveUnreachable puzzle

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

puzzle :: Puzzle
puzzle = readPuzzle $ unlines
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

solved :: Puzzle
solved = readPuzzle $ unlines
  [ "###.2#2.##"
  , "2.######2#"
  , "###2.#.#.#"
  , "#2.###2###"
  , "####.2#2.#"
  , "#.2#######"
  , "####.2#2#."
  , ".#2####.#2"
  , "2#.#.2####"
  , "#######.2#"
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

--
-- Old check functions
--

-- | Checks that there are no 2x2 areas of 'Black'.
checkPools :: Puzzle -> Status
checkPools p = let
  ((r0, c0), (r1, c1)) = bounds p
  poolTopLefts = range ((r0, c0), (r1 - 1, c1 - 1))
  isPool (r, c) = all (== Black) $ map (p !)
    [(r, c), (r + 1, c) , (r, c + 1), (r + 1, c + 1)]
  in if any isPool poolTopLefts
    then Impossible
    else Done

-- | Checks that there are no 'Empty' (unknown) squares.
checkDone :: Puzzle -> Status
checkDone p = if all (/= Empty) $ elems p
  then Done
  else Possible

-- | Checks that all 'Black's are connected to each other.
checkRivers :: Puzzle -> Status
checkRivers p = let
  blacks = Set.fromList [ i | (i, Black) <- assocs p ]
  unknown = Set.fromList [ i | (i, Empty) <- assocs p ]
  in if allConnected blacks
    then Done
    else if allConnected $ Set.union blacks unknown
      then Possible
      else Impossible

-- | Checks that all the given positions are connected to each other.
allConnected :: (Touching a) => Set a -> Bool
allConnected s = case Set.minView s of
  Nothing     -> True
  Just (h, _) -> s == grow (Set.singleton h) s

class (Ord a) => Touching a where
  isTouching :: a -> a -> Bool
  neighbors :: a -> Set a
  isTouching x y = Set.member x $ neighbors y

instance Touching Int where
  isTouching x y = abs (x - y) <= 1
  neighbors x = Set.fromList [x, x + 1, x - 1]

instance (Touching a, Touching b) => Touching (a, b) where
  neighbors (x, y) = Set.union
    (Set.map (, y) $ neighbors x)
    (Set.map (x ,) $ neighbors y)
  isTouching (a, b) (c, d) = or
    [ and [a == c, b `isTouching` d]
    , and [b == d, a `isTouching` c]
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

instance Monoid Status where
  mempty = Done
  mappend Impossible _          = Impossible
  mappend _          Impossible = Impossible
  mappend Done       Done       = Done
  mappend _          _          = Possible

distance :: Posn -> Posn -> Int
distance (r0, c0) (r1, c1) = abs (r0 - r1) + abs (c0 - c1)

unreachableBlack :: Posn -> Puzzle -> Maybe Puzzle
unreachableBlack sq p = let
  islands = [ i | (i, Island n) <- assocs p, distance sq i < n ]
  in guard (null islands) >> Just (p // [(sq, Black)])

tryBlacks :: Puzzle -> Puzzle
tryBlacks p = let
  go !z []         = z
  go !z (sq : sqs) = go (fromMaybe z $ unreachableBlack sq z) sqs
  in go p $ indices p

--
-- New solve functions
--

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
  sq <- Set.toList $ border z spread
  return (sq, Black)

eachWithRest :: [a] -> [(a, [a])]
eachWithRest []       = []
eachWithRest (x : xs) = (x, xs) : [ (y, x : ys) | (y, ys) <- eachWithRest xs ]

getUnreachable :: Puzzle -> Set Posn
getUnreachable z = let
  unknown = Set.fromList [ i | (i, Empty) <- assocs z ]
  blobDomains = do
    (Blob spread size, otherBlobs) <- eachWithRest $ getBlobs z
    let stayOut = unionMap neighbors $ Set.unions $ map blobSpread otherBlobs
    return (spread, size, Set.union spread $ Set.difference unknown stayOut)
    :: [(Set Posn, Int, Set Posn)]
  makeReach (blob, len, dom) =
    iterate (`growOnce` dom) blob !! (len - Set.size blob)
  in Set.difference unknown $ Set.unions $ map makeReach blobDomains

-- | Fills in 'Empty' squares with 'Black' if they can't possibly be reached
-- by any existing island.
solveUnreachable :: Puzzle -> Puzzle
solveUnreachable z = z // map (, Black) (Set.toList $ getUnreachable z)
