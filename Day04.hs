module Day04 where

import Control.Monad.Reader (ReaderT (ReaderT))
import AocUtils (readChar, readInt)

type Range = (Int, Int)

readRange :: ReaderT String Maybe Range
readRange = do
  x <- readInt
  readChar '-'
  y <- readInt
  return (x, y)

readRangePair :: ReaderT String Maybe (Range, Range)
readRangePair = do
  r1 <- readRange
  readChar ','
  r2 <- readRange
  return (r1, r2)

contains :: Range -> Range -> Bool
contains (x1, y1) (x2, y2) = x1 > x2 && y1 < y2