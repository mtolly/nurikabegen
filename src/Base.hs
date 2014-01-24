{-# LANGUAGE TupleSections #-}
module Base where

import Data.Array (assocs, listArray, Array, bounds, elems)
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Set as Set

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
