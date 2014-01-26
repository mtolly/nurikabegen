module Check
( Status(..)
, isFull
, checkAll
, Blob(..)
, getBlobs
) where

import Data.Array (bounds, range, (!), elems, assocs)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Set as Set

import Base
import Touching

-- | Used for checking various game rules.
data Status = Possible | Done | Impossible
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Monoid Status where
  mempty = Done
  mappend Impossible _          = Impossible
  mappend _          Impossible = Impossible
  mappend Done       Done       = Done
  mappend _          _          = Possible

isFull :: Puzzle -> Bool
isFull z = Done == checkFull z

checkAll :: Puzzle -> Status
checkAll z = mconcat $ map ($ z)
  [ checkFull
  , checkPools
  , checkRivers
  , checkBlobs
  , checkIslands
  ]

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
checkFull :: Puzzle -> Status
checkFull p = if all (/= Empty) $ elems p
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

-- | Representation of a (complete or incomplete) island and its squares.
data Blob = Blob
  { blobSpread :: Set Posn
  , blobSize   :: Int
  } deriving (Eq, Ord, Show, Read)

-- | Finds all the islands and the squares they currently own.
getBlobs :: Puzzle -> [Blob]
getBlobs z = let
  allLand = Set.fromList [ i | (i, sq) <- assocs z, notElem sq [Black, Empty] ]
  in do
    (i, n) <- [ (i, n) | (i, Island n) <- assocs z ]
    return $ Blob
      { blobSpread = grow (Set.singleton i) allLand
      , blobSize   = n
      }

-- | Checks that every island has its correct size. `Possible` means there is
-- an island with less than its expected size. `Impossible` means there is an
-- island with more than its expected size.
checkBlobs :: Puzzle -> Status
checkBlobs z = let
  ords = [ compare (Set.size spread) size | Blob spread size <- getBlobs z ]
  in if any (== GT) ords
    then Impossible
    else if any (== LT) ords
      then Possible
      else Done

-- | Checks various facts about all the island squares as a whole.
checkIslands :: Puzzle -> Status
checkIslands z = let
  blobs = getBlobs z
  sumSpread = sum    $ map (Set.size . blobSpread) blobs
  sumIsland = length $ filter (`notElem` [Black, Empty]) $ elems z
  sumGoal   = sum    $ map blobSize blobs
  in if areDistinct $ map blobSpread blobs
    then case (compare sumSpread sumIsland, compare sumIsland sumGoal) of
      (EQ, EQ) -> Done
      (GT, _ ) -> Impossible -- blobs overlap
      (_ , GT) -> Impossible -- more island squares than sum of numbers
      (LT, LT) -> Possible
      (LT, EQ) -> Impossible -- islands match goal, but spreads aren't right
      (EQ, LT) -> Possible
    else Impossible

areDistinct :: (Ord a) => [Set a] -> Bool
areDistinct = go Set.empty where
  go _      []       = True
  go marked (x : xs) = let
    marked' = Set.union marked x
    in if Set.size marked' == Set.size marked + Set.size x
      then go marked' xs
      else False
