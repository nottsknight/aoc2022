module Utils (readFileLines, readFileData, msort, dropSpace) where

import Data.Char (isSpace)

readFileLines :: String -> IO [String]
readFileLines fname = do
  content <- readFile fname
  return $ lines content

readFileData :: Read a => String -> IO [a]
readFileData fname = fmap (map read) (readFileLines fname)

dropSpace :: String -> String
dropSpace = dropWhile isSpace

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where
    (ys, zs) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys