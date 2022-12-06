module Utils.Graph (Graph (Graph), buildGraph, printGraph) where

import Control.Monad.State (State, evalState, gets, modify)
import Data.Set (Set, fromList, toList)
import Utils.Sort (msort)

type AdjList v = [(v, [v])]

data Graph v = Graph
  { vertices :: Set v,
    adjacencyList :: AdjList v
  }

printAdjList :: Show v => AdjList v -> IO ()
printAdjList [] = return ()
printAdjList ((x, xs) : xss) = do
  putStr $ show x ++ " -> "
  print xs
  printAdjList xss

printGraph :: Show v => Graph v -> IO ()
printGraph g = do
  print $ toList $ vertices g
  printAdjList $ adjacencyList g

buildAdjList :: Ord v => State [(v, v)] [(v, [v])]
buildAdjList = do
  n <- gets length
  if n == 0
    then return []
    else do
      x <- gets $ fst . head
      vs <- adj' x
      vss <- buildAdjList
      return $ (x, vs) : vss

adj' :: Ord v => v -> State [(v, v)] [v]
adj' x = do
  es <- gets $ takeWhile ((== x) . fst)
  modify $ dropWhile ((== x) . fst)
  return $ map snd es

buildVertexSet :: Ord v => [(v, v)] -> Set v
buildVertexSet = fromList . vertexList
  where
    vertexList [] = []
    vertexList ((v1, v2) : vs) = v1 : v2 : vertexList vs

buildGraph :: Ord v => [(v, v)] -> Graph v
buildGraph es = Graph (buildVertexSet es) (evalState buildAdjList es)