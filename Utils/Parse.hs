module Utils.Parse (dropSpace, parseInt, parseChar, parseString) where

import Control.Monad.State (StateT (StateT))
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)

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

parseString :: String -> StateT String Maybe String
parseString s = StateT $ \cs ->
  let cs' = dropSpace cs
   in if s `isPrefixOf` cs'
        then Just (s, drop (length s) cs')
        else Nothing

dropSpace :: String -> String
dropSpace = dropWhile isSpace
