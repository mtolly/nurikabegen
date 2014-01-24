{-# LANGUAGE TupleSections #-}
module Base where

import Data.Array (assocs, listArray, Array, bounds, elems, range, (!))
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Set as Set

import Touching

isComplete :: Puzzle -> Bool
isComplete z = null [ i | (i, Empty) <- assocs z ]

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
  Nothing -> True
  Just (h, _) -> s == grow (Set.singleton h) s
