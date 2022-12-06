module Utils.Parse
  ( Parser,
    dropSpace,
    parseInt,
    parseChar,
    parseString,
    parseAnyChar,
    empty,
    (<|>)
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.State (StateT (StateT), put, gets, modify)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)

-- | A state transformer that parses `Maybe a`s from a `String` input
type Parser a = StateT String Maybe a

-- | Returns an integer from the front of the input, if one exists.
parseInt :: Parser Int
parseInt = do
  cs <- gets $ takeWhile isDigit
  case cs of
    [] -> empty
    ns -> do modify $ drop (length ns)
             return $ read ns

-- | Returns a single character from the front of the input.
parseChar :: Char -> Parser Char
parseChar c = do
  cs <- gets dropSpace
  case cs of
    "" -> empty
    (c':cs') -> if c == c'
      then do put cs'; return c'
      else empty

-- | Returns the first non-whitespace character from the front of the input.
parseAnyChar :: Parser Char
parseAnyChar = do
  cs <- gets dropSpace
  case cs of
    "" -> empty
    (c:c'') -> do put c''; return c

-- | Returns the input string if it exists at the head of the input.
parseString :: String -> Parser String
parseString s = do
  cs <- gets dropSpace
  if s `isPrefixOf` cs
    then do modify $ drop (length s); return s
    else empty

-- | Remove all whitespace characters from the head of the input.
dropSpace :: String -> String
dropSpace = dropWhile isSpace
