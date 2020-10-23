# java-to-html-hs

A work in progress

I'm learning Haskell. To give me something concrete, I'm working on a program that
will take in Java code and spit out formatted HTML. I'm working through the book
*Language Implementation Patterns* as I go, porting the code as I find it there to
Haskell.

## Running Locally

On the command line, type in:

    stack build
    echo "<some text>" | stack exec java-to-html-hs-exe

## Complete

- Pattern 2 of the book, a recursive-descent lexer, with a very small grammar. A comma-separated list surrounded by brackets.

## In Development 

- Pattern 3 of the book, an LL1 parser to validate the tokens parsed by the lexer.