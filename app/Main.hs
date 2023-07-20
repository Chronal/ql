module Main where

import Text.Parsec

data Expr
  = App Expr Expr
  | Lam Char Expr
  | Var Char
  deriving (Show)

instance Eq Expr where
  (==) (Var x) (Var y) = x == y
  (==) (Lam c0 e0) (Lam c1 e1) = (c0 == c1) && (e0 == e1)
  (==) (App a0 a1) (App b0 b1) = (a0 == b0) && (a1 == b1)
  (==) _ _ = False

-- instance Show Expr where
--   show (Var x) = [x]
--   show (Lam c e) = "(\\" ++ [c] ++ "." ++ show e ++ ")"
--   show (App e0 e1) = show e0 ++ show e1

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
-- A (overly) simple evaluator that just replaces the bound variable
-- with the argument without any concern for name collisions
isval (Lam _ _) = True
isval _ = False

eval1 :: Expr -> Expr
eval1 (Var x) = Var x
eval1 (App (Lam c e) arg) | isval arg = eval1 (subst c e arg)
eval1 (App v0 e1) | isval v0 = App v0 (eval1 e1)
eval1 (App e0 e1) = App (eval1 e0) e1
eval1 e = e

eval e | e == t' = t'
       | otherwise = eval t'
  where t' = eval1 e

subst :: Char -> Expr -> Expr -> Expr
subst v (Var x) arg
  | v == x = arg
  | otherwise = Var x
subst v (Lam c e) arg = Lam c (subst v e arg)
subst v (App e0 e1) arg = App (subst v e0 arg) (subst v e1 arg)

-- Testing Expressions
-- identity = Lam 'x' (Var 'x')
-- zero = Lam 'a' (Lam 'b' (Var 'b'))
-- succ = Lam 'n' (Lam 'f' (Lam 'x' (App (Var 'f') (App (Var 'n') (App (Var 'f') (Var 'x'))))))
-- one = App Main.succ zero

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
  let tree = parse expr "" contents
  case tree of
    Left err -> print err
    Right e -> print (eval e)
