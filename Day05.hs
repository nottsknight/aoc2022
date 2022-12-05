{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day05 where

import Control.Monad.State (State, evalState, evalStateT, execState, state)
import Data.Maybe (fromJust)
import Utils.IO (readFileLines)
import Utils.Parse
  ( Parser,
    parseAnyChar,
    parseChar,
    parseInt,
    parseString,
  )

type Crate = Char

type Stack = [Crate]

type StackEnv = [Stack]

transfer :: Int -> Stack -> Stack -> (Stack, Stack)
transfer 0 from to = (from, to)
transfer _ [] to = error "Cannot transfer from empty stack"
transfer n (f : from) to = transfer (n -1) from (f : to)

replaceStack :: Int -> Stack -> StackEnv -> StackEnv
replaceStack _ s [] = [s]
replaceStack 0 s (_ : env) = s : env
replaceStack n s (e : env) = e : replaceStack (n -1) s env

data Move = Move {qty :: Int, from :: Int, to :: Int}

parseCrate :: Parser Crate
parseCrate = do
  parseChar '['
  c <- parseAnyChar
  parseChar ']'
  return c

parseMove :: Parser Move
parseMove = do
  parseString "move"
  qty <- parseInt
  parseString "from"
  from <- parseInt
  parseString "to"
  Move qty from <$> parseInt

applyMove :: Move -> State StackEnv ()
applyMove (Move qty fromIdx toIdx) = state $ \env ->
  let fromStack = env !! fromIdx
      toStack = env !! toIdx
      (fromStack', toStack') = transfer qty fromStack toStack
      env' = replaceStack fromIdx fromStack' env
   in ((), replaceStack toIdx toStack' env')

applyMoves :: [Move] -> State StackEnv ()
applyMoves [] = state $ \env -> ((), env)
applyMoves (m : ms) = do
  applyMove m
  applyMoves ms

main :: IO ()
main = do
  inputTxt <- readFileLines "data/day05.txt"
  let crateInput = takeWhile (/= "") inputTxt
  let moveInput = tail $ dropWhile (/= "") inputTxt
  let moves = map (fromJust . evalStateT parseMove) moveInput
  print ""
