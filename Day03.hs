module Day03 where

import Data.Bifunctor (bimap)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Utils (msort, readFileLines)

splitCompartments :: String -> (String, String)
splitCompartments s = splitAt n s where n = length s `div` 2

rucksacks :: [String] -> [(String, String, String)]
rucksacks (x : y : z : xs) = (x, y, z) : rucksacks xs
rucksacks _ = []

trimap :: (a -> b) -> (a -> b) -> (a -> b) -> (a, a, a) -> (b, b, b)
trimap f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)

findMatch :: String -> String -> Maybe Char
findMatch "" _ = Nothing
findMatch _ "" = Nothing
findMatch (c1 : cs1) (c2 : cs2)
  | c1 == c2 = Just c1
  | c1 < c2 = findMatch cs1 (c2 : cs2)
  | otherwise = findMatch (c1 : cs1) cs2

-- x y z
-- x z y
-- y x z
-- y z x
-- z x y
-- z y x

findMatch3 :: String -> String -> String -> Maybe Char
findMatch3 "" _ _ = Nothing
findMatch3 _ "" _ = Nothing
findMatch3 _ _ "" = Nothing
findMatch3 (x : xs) (y : ys) (z : zs)
  | x == y && y == z = Just x
  | otherwise =
    let n = minimum [x, y, z]
     in if n == x
          then findMatch3 xs (y:ys) (z:zs)
          else
            if n == y
              then findMatch3 (x:xs) ys (z:zs)
              else findMatch3 (x:xs) (y:ys) zs

--   | x < y && y < z = findMatch3 xs (y : ys) (z : zs)
--   | y < z = findMatch3 (x : xs) ys (z : zs)
--   | otherwise = findMatch3 (x : xs) (y : ys) zs

priority :: Char -> Int
priority c
  | c `elem` ['A' .. 'Z'] = ord c - 64 + 26
  | c `elem` ['a' .. 'z'] = ord c - 96
  | otherwise = -1

main :: IO ()
main = do
  inputs <- readFileLines "data/day03.txt"
  let rucksackGroups = map (trimap msort msort msort) $ rucksacks inputs
  let badges = map (\(x, y, z) -> fromJust $ findMatch3 x y z) rucksackGroups
  let priorities = map priority badges
  print $ sum priorities