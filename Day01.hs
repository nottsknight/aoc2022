{-# LANGUAGE LambdaCase #-}

module Day01 where

import Control.Monad.State (State, evalState, runState, state)
import Utils (msort, readFileLines)

getInputs :: State [String] [[String]]
getInputs = state $ \case
  [] -> return []
  ins ->
    let (in1, ins') = runState getSingleInput ins
        (ins'', ins''') = runState getInputs ins'
     in (in1 : ins'', ins''')

getSingleInput :: State [String] [String]
getSingleInput = state $ \case
  [] -> return []
  (cs : css) -> case cs of
    "" -> ([], css)
    cs' -> let (cs'', inp) = runState getSingleInput css in (cs' : cs'', inp)

main :: IO ()
main = do
  content <- readFileLines "data/day01.txt"
  let inputs = evalState getInputs content
  let counts = map (map read) inputs :: [[Int]]
  let sums = msort $ map sum counts
  print $ sum . take 3 . reverse $ sums