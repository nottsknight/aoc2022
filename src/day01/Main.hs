{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State (State, evalState, runState, state)
import Utils.IO (readFileLines)
import Utils.Sort (msort)

getInputs :: State [String] [[String]]
getInputs = state $ \case
  [] -> return []
  input ->
    let (counts, input') = runState getSingleInput input
        (countss, input'') = runState getInputs input'
     in (counts : countss, input'')

getSingleInput :: State [String] [String]
getSingleInput = state $ \case
  [] -> return []
  ("" : css) -> ([], css)
  (cs : css) -> let (cs', inp) = runState getSingleInput css in (cs : cs', inp)

main :: IO ()
main = do
  content <- readFileLines "data/day01.txt"
  let inputs = evalState getInputs content
  let counts = map (map read) inputs :: [[Int]]
  let sums = msort $ map sum counts
  print $ sum . take 3 . reverse $ sums