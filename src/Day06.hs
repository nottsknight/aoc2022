module Day06 where

import Data.List (permutations)

findMarker :: String -> Int
findMarker = find' 4

find' :: Int -> String -> Int
find' n (a : b : c : d : cs) =
  if allDistinct [a, b, c, d]
    then n
    else find' (n + 1) (b : c : d : cs)
find' _ _ = -1

allDistinct :: Eq a => [a] -> Bool
allDistinct xs =
  let insts = map (`countInstances` xs) xs
   in all (== 1) insts

countInstances :: Eq a => a -> [a] -> Int
countInstances x = length . filter (== x)

findMessage :: String -> Int
findMessage = find'' 14

find'' :: Int -> String -> Int
find'' n cs =
  if allDistinct (take 14 cs)
    then n
    else find'' (n + 1) (tail cs)

main :: IO ()
main = do
  input <- readFile "data/day06.txt"
  print $ findMessage input