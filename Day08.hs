module Day08 where

import Data.Char (digitToInt)
import Data.List (transpose)
import Utils.IO (readFileLines)

type TreeGrid = [[Int]]

type VisGrid = [[Bool]]

genTreeGrids :: TreeGrid -> [TreeGrid]
genTreeGrids grid = [grid, gridRL, gridUD, gridDU]
  where
    gridRL = map reverse grid
    gridUD = transpose grid
    gridDU = map reverse gridUD

markVisible :: [Int] -> [Bool]
markVisible [] = []
markVisible (t : ts) = True : vis' t ts

vis' :: Int -> [Int] -> [Bool]
vis' _ [] = []
vis' t0 (t : ts)
  | t > t0 = True : vis' t ts
  | otherwise = False : vis' t0 ts

getVisGrid :: TreeGrid -> VisGrid
getVisGrid = map markVisible

listOr :: [Bool] -> [Bool] -> [Bool]
listOr [] _ = []
listOr _ [] = []
listOr (b1 : bs1) (b2 : bs2) = (b1 || b2) : listOr bs1 bs2

cols :: [[a]] -> [[a]]
cols [] = []
cols xss = map head xss : cols (map tail xss)

mergeGrids :: VisGrid -> VisGrid -> VisGrid
mergeGrids g1 g2 = [listOr l r | (l, r) <- zip g1 g2]

mergeGridList :: [VisGrid] -> VisGrid
mergeGridList [g] = g
mergeGridList [g1, g2] = mergeGrids g1 g2
mergeGridList gs = mergeGridList [mergeGridList ls, mergeGridList rs]
  where
    n = length gs `div` 2
    ls = take n gs
    rs = drop n gs

countTrues :: [[Bool]] -> Int
countTrues bs = sum $ map (length . filter id) bs

main :: IO ()
main = do
  input <- readFileLines "data/day08.txt"
  let gridLR = map (map digitToInt) input
  let treeGrids = genTreeGrids gridLR
  let [vis1,vis2,vis3,vis4] = map getVisGrid treeGrids
  let combined = mergeGridList [vis1, map reverse vis2, cols vis3, cols (map reverse vis4)]
  print $ countTrues combined