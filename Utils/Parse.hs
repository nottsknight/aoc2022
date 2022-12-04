module Utils.Parse (dropSpace, parseInt, parseChar) where

import Control.Monad.State (StateT (StateT))
import Data.Char (isDigit, isSpace)

parseInt :: StateT String Maybe Int
parseInt = StateT $ \cs ->
  let cs' = dropSpace cs
   in case takeWhile isDigit cs' of
        [] -> Nothing
        ns -> Just (read ns, drop (length ns) cs')

parseChar :: Char -> StateT String Maybe Char
parseChar c = StateT $ \cs ->
  let (c' : cs') = dropSpace cs
   in if c == c' then Just (c, cs') else Nothing

dropSpace :: String -> String
dropSpace = dropWhile isSpace
