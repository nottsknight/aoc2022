module Day07 where

import Control.Monad.State (State, get, gets, put)
import Data.Maybe (fromJust)
import Utils.Parse

data FSNode
  = File String FSNode Integer
  | Dir String (Maybe FSNode) [FSNode]

name :: FSNode -> String
name (File x _ _) = x
name (Dir x _ _) = x

size :: FSNode -> Integer
size (File _ _ x) = x
size (Dir _ _ xs) = sum $ map size xs

parent :: FSNode -> Maybe FSNode
parent (File _ p _) = Just p
parent (Dir _ p _) = p

children :: FSNode -> [FSNode]
children (File _ _ _) = []
children (Dir _ _ xs) = xs

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst pred (x : xs) = if pred x then Just x else findFirst pred xs

data FileSystem = FS {rootDir :: FSNode, workingDir :: FSNode}

cd :: String -> State FileSystem ()
cd "/" = do
  root <- gets rootDir
  put $ FS root root
  return ()
cd ".." = do
  FS root wd <- get
  case parent wd of
    Nothing -> return ()
    Just p -> do
      put $ FS root (fromJust . parent $ wd)
      return ()
cd dirname = do
  FS root wd <- get
  let nextDir = findFirst ((== dirname) . name) (children wd)
  case nextDir of
    Nothing -> error $ dirname ++ " does not exist"
    Just dir -> do
      put $ FS root dir
      return ()
