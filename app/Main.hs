module Main where

import Lexer ( tokenize )

main :: IO ()
main = do
    input <- getContents
    print $ tokenize input