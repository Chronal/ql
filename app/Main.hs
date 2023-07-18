module Main where

import Text.Parsec


data Term = Var | Abstraction Term | Application Term Term

main :: IO ()
main = putStrLn "Hello, World!"
