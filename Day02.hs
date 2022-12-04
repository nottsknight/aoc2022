module Day02 where

import Control.Monad.State (StateT (StateT), evalStateT, runStateT)
import Utils.Parse (Parser, dropSpace)

data Move = Rock | Paper | Scissors deriving (Eq)

moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

moveLoseTo :: Move -> Move
moveLoseTo Rock = Paper
moveLoseTo Paper = Scissors
moveLoseTo Scissors = Rock

moveWinTo :: Move -> Move
moveWinTo Rock = Scissors
moveWinTo Paper = Rock
moveWinTo Scissors = Paper

data Result = Win | Loss | Draw

strategyScore :: (Move, Move) -> Int
strategyScore (m1, m2)
  | m1 == m2 = 3
  | m1 `defeats` m2 = 0
  | otherwise = 6

defeats :: Move -> Move -> Bool
defeats Paper Rock = True
defeats Scissors Paper = True
defeats Rock Scissors = True
defeats _ _ = False

readMove :: Parser Move
readMove = StateT $ \cs ->
  let cs' = dropSpace cs
   in case cs' of
        ('A' : cs) -> Just (Rock, cs)
        ('B' : cs) -> Just (Paper, cs)
        ('C' : cs) -> Just (Scissors, cs)
        _ -> Nothing

readResult :: Parser Result
readResult = StateT $ \cs ->
  let cs' = dropSpace cs
   in case cs' of
        ('X' : cs) -> Just (Loss, cs)
        ('Y' : cs) -> Just (Draw, cs)
        ('Z' : cs) -> Just (Win, cs)
        _ -> Nothing

readStrategy :: Parser (Move, Result)
readStrategy = do
  m1 <- readMove
  m2 <- readResult
  return (m1, m2)

parseGoals :: String -> [(Move, Result)]
parseGoals input = case runStateT readStrategy input of
  Nothing -> []
  Just (xy, input') -> let xs = parseGoals input' in xy : xs

pickStrategy :: (Move, Result) -> (Move, Move)
pickStrategy (m, Draw) = (m, m)
pickStrategy (m, Loss) = (m, moveWinTo m)
pickStrategy (m, Win) = (m, moveLoseTo m)

main :: IO ()
main = do
  input <- readFile "data/day02.txt"
  let goals = parseGoals input
  let strategies = map pickStrategy goals
  let moveScores = map (moveScore . snd) strategies
  let strategyScores = map strategyScore strategies
  print $ sum moveScores + sum strategyScores