module Utils.Parse
  ( Parser,
    dropSpace,
    parseInt,
    parseInteger,
    parseChar,
    parseString,
    parseAnyChar,
    parseAnyString,
    empty,
    (<|>),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.State (StateT (StateT), gets, modify)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)

-- | A state transformer that parses `Maybe a`s from a `String` input
type Parser a = StateT String Maybe a

-- | Returns an integer from the front of the input, if one exists.
parseInt :: Parser Int
parseInt = StateT $ \cs ->
  let cs' = dropSpace cs
   in case takeWhile isDigit cs' of
        [] -> Nothing
        ns -> Just (read ns, drop (length ns) cs')

parseInteger :: Parser Integer
parseInteger = StateT $ \cs ->
  let cs' = dropSpace cs
   in case takeWhile isDigit cs' of
        [] -> Nothing
        ns -> Just (read ns, drop (length ns) cs')

-- | Returns a single character from the front of the input.
parseChar :: Char -> Parser Char
parseChar c = StateT $ \cs ->
  let cs' = dropSpace cs
   in case cs' of
        "" -> Nothing
        (c' : cs'') -> if c == c' then Just (c, cs'') else Nothing

-- | Returns the first non-whitespace character from the front of the input.
parseAnyChar :: Parser Char
parseAnyChar = StateT $ \cs ->
  let cs' = dropSpace cs
   in case cs' of
        "" -> Nothing
        (c : cs'') -> Just (c, cs'')

-- | Returns the input string if it exists at the head of the input.
parseString :: String -> Parser String
parseString s = StateT $ \cs ->
  let cs' = dropSpace cs
   in if s `isPrefixOf` cs'
        then Just (s, drop (length s) cs')
        else Nothing

parseAnyString :: Parser String
parseAnyString = do
  s <- gets $ takeWhile (not . isSpace) . dropSpace
  case s of
    "" -> empty
    cs -> do
      modify $ dropSpace . dropWhile (not . isSpace)
      return s

-- | Remove all whitespace characters from the head of the input.
dropSpace :: String -> String
dropSpace = dropWhile isSpace
