module Main where

import Base
import Solve

main :: IO ()
main = interact $ showPuzzle . fst . solve . readPuzzle
