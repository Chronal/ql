module Main where

import Text.Parsec

data Expr = App Expr Expr
          | Lam Char Expr
          | Var Char deriving (Show)

-- Parsing
-- This lambda calc parser is almost exactly the same as the one
-- in Hutton and Meijer's paper "Monadic Parser Combinators"
-- except that it's built on the parsec library

expr = atom `chainl1` return App
atom = lam <|> var <|> paren

lam = do
  _ <- char '\\'
  v <- lower
  _ <- char '.'
  Lam v <$> expr

var = do
  Var <$> lower

paren = between (char '(') (char ')') expr

-- Interpreter
-- Printing Lambda Terms
-- generating an infinite amount of variable names
-- alpha :: [Char]
-- alpha = [ chr (x + ord 'a') | x <-[0..25] ]
-- naturals :: [Integer]
-- naturals = [0..]
-- names :: [[Char]]
-- names = concatMap (\n -> [ c : show n |  c <- alpha] ) naturals

main :: IO ()
main = do
          putStr "Enter file name: "
          fileName <- getLine
          contents <- readFile fileName
          parseTest expr contents
