module Main where

import Text.Parsec

data Expr
  = App Expr Expr
  | Lam Char Expr
  | Var Char
  deriving Show

instance Eq Expr where
  (==) (Var x) (Var y) = x == y
  (==) (Lam c0 e0) (Lam c1 e1) = (c0 == c1) && (e0 == e1)
  (==) (App a0 a1) (App b0 b1) = (a0 == b0) && (a1 == b1)
  (==) _ _ = False

exprToString (Var x) = [x]
exprToString (Lam c e) = "(\\" ++ [c] ++ "." ++ exprToString e ++ ")"
exprToString (App e0 e1) = exprToString e0 ++ exprToString e1

printExpr e = putStrLn (exprToString e)


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
isReduced (Var _) = True
isReduced (App (Lam c e) arg) = False
isReduced (App e0 e1) = isReduced e0 && isReduced e1
isReduced (Lam c e) = isReduced e

eval1 :: Expr -> Expr
eval1 (App (Lam c e) arg) | not (isReduced arg) = App (Lam c e) (eval1 arg)
                          | not (isReduced e) = App (Lam c (eval1 e)) arg
                          | otherwise = subst c e (eval1 arg)
eval1 (App e0 e1) | isReduced e0 = App e0 (eval1 e1)
                  | otherwise = App (eval1 e0) e1
eval1 (Lam c e) = Lam c (eval1 e)
eval1 (Var x) = Var x

eval e | e == t' = t'
       | otherwise = eval t'
  where t' = eval1 e

subst :: Char -> Expr -> Expr -> Expr
subst v (Var x) arg
  | v == x = arg
  | otherwise = Var x
subst v (Lam c e) arg = Lam c (subst v e arg)
subst v (App e0 e1) arg = App (subst v e0 arg) (subst v e1 arg)


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
    Right e -> printExpr e >> printExpr (eval e)
