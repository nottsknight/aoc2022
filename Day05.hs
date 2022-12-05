module Day05 where

import Control.Monad.State
import Data.Maybe (fromJust, isJust, catMaybes, mapMaybe)
import Utils.IO (readFileLines)
import Utils.Parse
import Data.List (transpose)
import Debug.Trace (trace)

type Crate = Char

type Stack = [Crate]

type StackEnv = [Stack]

split4s :: String -> [String]
split4s "" = []
split4s cs = l : split4s rs
  where
    (l, rs) = splitAt 4 cs

data Move = Move {qty :: Int, from :: Int, to :: Int}

parseCrate :: Parser Crate
parseCrate = do
  parseChar '['
  c <- parseAnyChar
  parseChar ']'
  return c

buildCrateRow :: String -> [Maybe Crate]
buildCrateRow = map (evalStateT parseCrate) . split4s

buildStackEnv :: [[Maybe Crate]] -> StackEnv
buildStackEnv = map (reverse . catMaybes) . transpose

parseMove :: Parser Move
parseMove = do
  parseString "move"
  qty <- parseInt
  parseString "from"
  from <- parseInt
  parseString "to"
  Move qty from <$> parseInt

applyMove :: Move -> State StackEnv ()
applyMove (Move qty fromIdx toIdx) = do
  s1 <- getStack fromIdx
  s2 <- getStack toIdx
  let (s1',s2') = transfer' qty s1 s2
  doReplace fromIdx s1'
  doReplace toIdx s2'

getStack :: Int -> State StackEnv Stack
getStack n = gets (!! (n-1))

transfer' :: Int -> Stack -> Stack -> (Stack, Stack)
transfer' 0 from to = (from, to)
transfer' _ [] to = ([], to)
transfer' n (f : from) to = transfer' (n -1) from (f : to)

doReplace :: Int -> Stack -> State StackEnv ()
doReplace n s = do
  env <- get
  put $ replace' n s env

replace' :: Int -> Stack -> StackEnv -> StackEnv
replace' _ s [] = [s]
replace' 0 s (_ : env) = s : env
replace' n s (e : env) = e : replace' (n -1) s env

applyMoves :: [Move] -> State StackEnv ()
applyMoves [] = return ()
applyMoves (m : ms) = do
  applyMove m
  applyMoves ms

stackTops :: StackEnv -> String
stackTops [] = ""
stackTops ([]:ss) = stackTops ss
stackTops (s:ss) = head s : stackTops ss

main :: IO ()
main = do
  inputTxt <- readFileLines "data/day05.txt"
  let crateInput = init $ takeWhile (/= "") inputTxt
  let startState = buildStackEnv $ map buildCrateRow crateInput
  let moveInput = tail $ dropWhile (/= "") inputTxt
  let moves = mapMaybe (evalStateT parseMove) moveInput
  let endState = execState (applyMoves moves) startState
  print $ stackTops endState
