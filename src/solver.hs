module Main where

import Base
import Check (checkAll)
import Solve

main :: IO ()
main = interact $ \s -> let
  z = solve $ readPuzzle s
  in unlines [showPuzzle z, show $ checkAll z]
