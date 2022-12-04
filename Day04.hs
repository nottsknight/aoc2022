module Day04 where

import Control.Monad.State (StateT (StateT), runStateT)
import Utils.Parse (parseChar, parseInt)

type Assignment = (Int, Int)

parseAssignment :: StateT String Maybe Assignment
parseAssignment = do
  x <- parseInt
  parseChar '-'
  y <- parseInt
  return (x, y)

parseAssignmentPair :: StateT String Maybe (Assignment, Assignment)
parseAssignmentPair = do
  r1 <- parseAssignment
  parseChar ','
  r2 <- parseAssignment
  return (r1, r2)

getAssignments :: String -> [(Assignment, Assignment)]
getAssignments input = case runStateT parseAssignmentPair input of
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