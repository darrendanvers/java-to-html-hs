module Parser (validate) where

import Lexer

-- Given a token, says if its the beginning of a list.
listStart :: Token -> Bool
listStart (tt, _) = tt == Lexer.LeftBracket

-- Given a token, says if its the end of a list.
listEnd :: Token -> Bool
listEnd (tt, _) = tt == Lexer.RightBracket

-- Given a token, says if its the end of the input.
lineEnd :: Token -> Bool
lineEnd (tt, _) = tt == Lexer.End

-- Given a token, says if its a value in a list.
listValue :: Token -> Bool
listValue (tt, _) = tt == Lexer.Character

-- Given a token, says if its a value separator.
listSeparator :: Token -> Bool
listSeparator (tt, _) = tt == Lexer.Comma

-- These functions all work together to determine if a line of input is valid.
-- Valid input starts with a '[', has any number of values separated by ',', and ends
-- with a ']'.

-- Determines if a token is either the end of the list or a list separator (and the list
-- should then have a value).
separatorOrEnd :: [Token] -> Bool
separatorOrEnd [] = error "Expected ','' at end of list."
separatorOrEnd (x:xs)
    | listEnd x = atLineEnd xs
    | listSeparator x = value xs
    | otherwise = error ("Expected ','' at " ++ show x)

-- Determines if this is the last token and it contains the end character.
atLineEnd :: [Token] -> Bool
atLineEnd [] = error "Expected line end."
atLineEnd (x:xs) = lineEnd x && null xs

-- Determines if this token is a value. It should then be followed by a separator
-- or the end of the list.
value :: [Token] -> Bool
value [] = error "Expected value at end of list."
value (x:xs)
    | listValue x = separatorOrEnd xs
    | otherwise = error ("Expected value at " ++ show x)
    
-- Determines if we are at the end of the list or if we are looking at a value. If its
-- a value, it should be followed by a separator or the the end of the list.    
valueOrEnd :: [Token] -> Bool
valueOrEnd [] = error "Expected character or ']' at end of list."
valueOrEnd (x:xs)
    | listEnd x = atLineEnd xs
    | listValue x = separatorOrEnd xs
    | otherwise = error ("Expected character or ']' at " ++ show x)

-- The exposed validation function that will check to make sure a list starts with a '['
-- and then kicks off the rest of the validation checks.
validate :: [Token] -> Bool
validate [] = True
validate (x:xs) 
    | listStart x = valueOrEnd xs
    | otherwise = error ("Expected '[' at " ++ show x)