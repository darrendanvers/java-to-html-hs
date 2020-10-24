module Lexer 
    ( TokenType(..)
    , Token
    , tokenize
    ) where

import Data.Char

-- The types of tokens the grammar understands.
data TokenType = LeftBracket | RightBracket | Comma | Character | End deriving (Show, Eq, Bounded, Enum)

-- All the tokens that should be treated as delimiters.
delimiters :: [TokenType]
delimiters = [LeftBracket, RightBracket, Comma]

-- Converts a String to a TokenType
toTokenType :: String -> TokenType
toTokenType t 
    | t == "[" = LeftBracket
    | t == "]" = RightBracket
    | t == "," = Comma
    | null t = End                -- A quirk I couldn't work around was an empty string after the end of the file. 
    | otherwise = Character

-- Type to store a Token. The token holds the type of token it is
-- and the raw value that was read in.
type Token = (TokenType, String)

-- Converts a String to a Token.
toToken :: String -> Token
toToken t = (toTokenType t, t)

-- Determines if a String is in the set of delimiters.
isDelimiter :: String -> Bool
isDelimiter c = toTokenType c `elem` delimiters

-- Takes a string and returns a tuple. The string will be split along
-- delimiter/non-delimiter lines. The first value in the tuple will
-- contain either a delimiter or whatever was at the beginning of the
-- string up to the next delimiter. THe second value will contain the
-- rest of the string.
--
-- For example:
-- "[xxx" -> ("[", "xxx")
-- "xxx]xxx" -> ("xxx", "]xxx")
tillNotReserved :: String -> (String, String)
tillNotReserved [] = ([], [])
tillNotReserved (x:xs)
    | isSpace x = ([], xs)                 -- Whitespace breaks works, but is not stored.
    | isDelimiter [x] = ([x], xs)          -- If the first character is a delimiter, return it by itself.
    | null xs = ([x], xs)                  -- If the remainder of the string is empty, just return the head.
    | isDelimiter [head xs] = ([x], xs)    -- If we're here, the first character is not a delimiter. If the next one
                                           -- is, we need to break the string.
    | otherwise = (x : y, z) where         -- For everything else, return the first character appended
        (y, z) = tillNotReserved xs        -- to the result of calling this function again with the remainder of the string.

-- Given a string, parse it into a series of tokens.
tokenize :: String -> [Token]
tokenize [] = []
tokenize s = toToken x : tokenize y where
    (x, y) = tillNotReserved s