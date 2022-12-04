module AocUtils (readFileLines, readFileData, msort, dropSpace, readInt, readChar) where

import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Char (isDigit, isSpace)

readFileLines :: String -> IO [String]
readFileLines fname = do
  content <- readFile fname
  return $ lines content

readFileData :: Read a => String -> IO [a]
readFileData fname = fmap (map read) (readFileLines fname)

readInt :: ReaderT String Maybe Int
readInt = ReaderT $ \cs ->
  let cs' = dropSpace cs
   in case takeWhile isDigit cs' of
        [] -> Nothing
        ns -> Just $ read ns

readChar :: Char -> ReaderT String Maybe Char
readChar c = ReaderT $ \cs ->
  let (c' : cs') = dropSpace cs
   in if c == c' then Just c else Nothing

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