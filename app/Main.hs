module Main where

import Lexer ( tokenize )
import Parser ( validate )

main :: IO ()
main = do
    input <- getContents
    let tokens = tokenize input
    print tokens
    let parses = validate tokens
    print parses
