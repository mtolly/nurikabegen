{-# LANGUAGE BangPatterns, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Array
import Data.Char (toUpper)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intersect, nub, (\\), union)
import Data.Monoid
import Control.Monad (guard)

import qualified Data.Set as Set
import Data.Set (Set)

--import Debug.Trace (trace)
--import System.Exit

main :: IO ()
main = putStrLn $ showPuzzle $
  solveCloseIslands $ solveNoPools $ solveCloseIslands $
  solveNoPools $ solveCloseIslands $ solveNoPools $ solveSeparateIslands $
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
  blacks = [ i | (i, Black) <- assocs p ]
  unknown = [ i | (i, Empty) <- assocs p ]
  in if allConnected blacks
    then Done
    else if allConnected $ blacks ++ unknown
      then Possible
      else Impossible

-- | Checks that all the given positions are connected to each other.
allConnected :: [Posn] -> Bool
allConnected [] = True
allConnected (h : t) = null $ snd $ grow [h] t

class (Eq a) => Touching a where
  isTouching :: a -> a -> Bool
  neighbors :: a -> [a]
  isTouching x y = elem x $ neighbors y

instance Touching Int where
  isTouching x y = abs (x - y) <= 1
  neighbors x = [x, x + 1, x - 1]

instance (Eq a, Eq b, Enum a, Enum b) => Touching (a, b) where
  isTouching (x0, y0) (x1, y1) = let
    dx = abs $ fromEnum x0 - fromEnum x1
    dy = abs $ fromEnum y0 - fromEnum y1
    in dx + dy <= 1
  neighbors (x, y) =
    [(x, y), (succ x, y), (pred x, y), (x, succ y), (x, pred y)]

grow :: (Touching a) => [a] -> [a] -> ([a], [a])
grow xs [] = (xs, [])
grow xs ys = case nub (concatMap neighbors xs) `intersect` ys of
  []     -> (xs, ys) -- growing is done
  shared -> let
    new = shared \\ xs
    in grow (xs ++ new) (ys \\ shared)

-- | Checks that every 'Island' has the right number of 'Dot's connected only
-- to itself and no other 'Island'.
checkIslands :: Puzzle -> Status
checkIslands p = let
  nonBlacks = [ i | (i, e) <- assocs p, e /= Black ]
  knownWhites = [ i | (i, e) <- assocs p, e `notElem` [Black, Empty] ]
  islands = [ (fst $ grow [i] nonBlacks, n) | (i, Island n) <- assocs p ]
  islands' = [ (fst $ grow [i] knownWhites, n) | (i, Island n) <- assocs p ]
  numbersPossible = all (\(xs, n) -> length xs >= n) islands
  numbersDone     = all (\(xs, n) -> length xs == n) islands
  nonOverlapping [] = True
  nonOverlapping (x : xs) =
    all (\y -> null $ intersect x y) xs && nonOverlapping xs
  in if nonOverlapping $ map fst islands'
    then if numbersDone
      then Done
      else if numbersPossible
        then Possible
        else Impossible
    else Impossible

instance Monoid Status where
  mempty = Done
  mappend Impossible _          = Impossible
  mappend _          Impossible = Impossible
  mappend Done       Done       = Done
  mappend _          _          = Possible

checkAll :: Puzzle -> Status
checkAll p = mconcat [checkDone p, checkIslands p, checkRivers p, checkPools p]

trySquare :: Posn -> Puzzle -> Maybe Puzzle
trySquare sq p = let
  withBlack = p // [(sq, Black)]
  withDot = p // [(sq, Dot)]
  in if (p ! sq) /= Empty
    then Nothing
    else case checkAll withBlack of
      Impossible -> Just withDot
      Done -> Just withBlack
      Possible -> case checkAll withDot of
        Impossible -> Just withBlack
        Done -> Just withDot
        Possible -> Nothing

trySolve :: Puzzle -> Puzzle
trySolve p = let
  sqs = indices p
  go z = case mapMaybe (`trySquare` z) sqs of
    []     -> z
    z' : _ -> go z'
  in go p

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
  { blobSpread :: [Posn]
  , blobSize   :: Int
  } deriving (Eq, Ord, Show, Read)

-- | Finds all the islands and the squares they currently own.
getBlobs :: Puzzle -> [Blob]
getBlobs z = let
  islandHeads = [ (i, n) | (i, Island n) <- assocs z ]
  allLand = [ i | (i, sq) <- assocs z, notElem sq [Black, Empty] ]
  in flip map islandHeads $ \(i, n) -> Blob
    { blobSpread = fst $ grow [i] allLand
    , blobSize   = n
    }

-- | Returns all 'Empty' squares next to the given 'blobSpread'.
border :: Puzzle -> [Posn] -> [Posn]
border z isl = filter (\n -> safeIndex z n == Just Empty) $
  nub $ concatMap neighbors isl

-- | Fills in 'Empty' squares with 'Black' which are next to finished islands.
solveCloseIslands :: Puzzle -> Puzzle
solveCloseIslands z = z // do
  Blob spread size <- getBlobs z
  guard $ length spread == size
  sq <- border z spread
  return (sq, Black)

-- | Fills in 'Empty' squares with 'Black' if they border two islands.
-- This is now subsumed by 'solveUnreachable'.
solveSeparateIslands :: Puzzle -> Puzzle
solveSeparateIslands z = z // let
  blobs = map blobSpread $ getBlobs z
  neighboringTwo i = let
    neighs = neighbors i
    in case filter (not . null . intersect neighs) blobs of
      _ : _ : _ -> True
      _         -> False
  in map (, Black) [ i | (i, Empty) <- assocs z, neighboringTwo i ]

eachWithRest :: [a] -> [(a, [a])]
eachWithRest []       = []
eachWithRest (x : xs) = (x, xs) : [ (y, x : ys) | (y, ys) <- eachWithRest xs ]

growOnce :: (Touching a) => [a] -> [a] -> [a]
growOnce xs ys = intersect ys $ nub $ concatMap neighbors xs

getUnreachable :: Puzzle -> [Posn]
getUnreachable z = let
  unknown = [ i | (i, Empty) <- assocs z ]
  blobDomains = do
    (Blob spread size, otherBlobs) <- eachWithRest $ getBlobs z
    let stayOut = nub $ concatMap neighbors $ concatMap blobSpread otherBlobs
    return (spread, size, union spread $ unknown \\ stayOut)
    :: [([Posn], Int, [Posn])]
  makeReach (blob, len, dom) =
    iterate (`growOnce` dom) blob !! (len - length blob)
  in unknown \\ nub (concatMap makeReach blobDomains)

-- | Fills in 'Empty' squares with 'Black' if they can't possibly be reached
-- by any existing island.
solveUnreachable :: Puzzle -> Puzzle
solveUnreachable z = z // map (, Black) (getUnreachable z)

isGrowable :: Puzzle -> Bool
isGrowable z = let
  unknown = [ i | (i, Empty) <- assocs z ]
  blobDomains = do
    (Blob spread size, otherBlobs) <- eachWithRest $ getBlobs z
    let stayOut = nub $ concatMap neighbors $ concatMap blobSpread otherBlobs
    return (spread, size, union spread $ unknown \\ stayOut)
    :: [([Posn], Int, [Posn])]
  checkBlob (blob, len, dom) = let
    res = iterate (`growOnce` dom) blob !! (len - length blob)
    newSection = res \\ blob
    in length newSection + len == length blob
  in all checkBlob blobDomains

solveWouldRuin :: Puzzle -> Puzzle
solveWouldRuin z = let
  blobArea = concatMap blobSpread $ getBlobs z
  unknown = [ i | (i, Empty) <- assocs z ]
  borderArea = intersect unknown $ nub $ concatMap neighbors blobArea
  checkSquare sq = if isGrowable $ z // [(sq, Black)]
    then Nothing
    else Just $ z // [(sq, Dot)]
  in case mapMaybe checkSquare borderArea of
    []     -> z
    z' : _ -> solveWouldRuin z'
