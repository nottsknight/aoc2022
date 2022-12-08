module Day08 where

import Data.Char (digitToInt)
import Data.List (transpose)
import Utils.IO (readFileLines)

markVisible :: [Int] -> [Bool]
markVisible [] = []
markVisible (t : ts) = True : vis' t ts

vis' :: Int -> [Int] -> [Bool]
vis' _ [] = []
vis' t0 (t : ts)
  | t > t0 = True : vis' t ts
  | otherwise = False : vis' t0 ts

listOr :: [Bool] -> [Bool] -> [Bool]
listOr [] _ = []
listOr _ [] = []
listOr (b1 : bs1) (b2 : bs2) = (b1 || b2) : listOr bs1 bs2

main :: IO ()
main = do
  input <- readFileLines "data/day08.txt"
  let gridLR = map (map digitToInt) input
  let visLR = map markVisible gridLR
  let gridRL = map reverse gridLR
  let visRL = map markVisible gridRL
  let gridUD = transpose gridLR
  let visUD = map markVisible gridUD
  let gridDU = map reverse gridUD
  let visDU = map markVisible gridDU
  print ""