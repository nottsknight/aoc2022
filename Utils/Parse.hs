module Utils.Parse
  ( Parser,
    dropSpace,
    parseInt,
    parseChar,
    parseString,
    parseAnyChar,
  )
where

import Control.Applicative (Alternative)
import Control.Monad.State (StateT (StateT))
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
  let (c' : cs') = dropSpace cs in Just (c', cs')

-- | Returns the input string if it exists at the head of the input.
parseString :: String -> Parser String
parseString s = StateT $ \cs ->
  let cs' = dropSpace cs
   in if s `isPrefixOf` cs'
        then Just (s, drop (length s) cs')
        else Nothing

-- | Remove all whitespace characters from the head of the input.
dropSpace :: String -> String
dropSpace = dropWhile isSpace
