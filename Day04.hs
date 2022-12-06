module Day04 where

import Control.Monad.State (StateT (StateT), evalStateT)
import Utils.Parse (Parser, parseChar, parseInt)

type Assignment = (Int, Int)

parseAssignment :: Parser Assignment
parseAssignment = do
  x <- parseInt
  parseChar '-'
  y <- parseInt
  return (x, y)

parseAssignmentPair :: Parser (Assignment, Assignment)
parseAssignmentPair = do
  r1 <- parseAssignment
  parseChar ','
  r2 <- parseAssignment
  return (r1, r2)

parseAssignments :: Parser [(Assignment,Assignment)]
parseAssignments = do
  as <- parseAssignmentPair
  ass <- parseAssignments
  return $ as : ass

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
  let (Just assignments) = evalStateT parseAssignments input
  let intersections = filter (uncurry overlaps) assignments
  print $ length intersections