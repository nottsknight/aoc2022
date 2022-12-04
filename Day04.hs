module Day04 where

import AocUtils (readChar, readInt)
import Control.Monad.State (StateT (StateT), runStateT)

type Assignment = (Int, Int)

readAssignment :: StateT String Maybe Assignment
readAssignment = do
  x <- readInt
  readChar '-'
  y <- readInt
  return (x, y)

readAssignmentPair :: StateT String Maybe (Assignment, Assignment)
readAssignmentPair = do
  r1 <- readAssignment
  readChar ','
  r2 <- readAssignment
  return (r1, r2)

getAssignments :: String -> [(Assignment, Assignment)]
getAssignments input = case runStateT readAssignmentPair input of
  Nothing -> []
  Just (rs, input') -> rs : getAssignments input'

fullyContains :: Assignment -> Assignment -> Bool
r1 `fullyContains` r2 = r1 `contains` r2 || r2 `contains` r1
  where
    (x1, y1) `contains` (x2, y2) = not (x1 < x2 || y1 > y2)

overlaps :: Assignment -> Assignment -> Bool
r1 `overlaps` r2 = overlap' r1 r2 || overlap' r2 r1
  where
    overlap' r1 r2 = not (condition1 r1 r2 && condition2 r1 r2)
    condition1 (x1, y1) (x2, y2) = x1 > x2 || y1 < x2
    condition2 (x1, y1) (x2, y2) = y1 < y2 || x1 > y2

main :: IO ()
main = do
  input <- readFile "data/day04.txt"
  let assignments = getAssignments input
  let intersections = filter (uncurry overlaps) assignments
  print $ length intersections