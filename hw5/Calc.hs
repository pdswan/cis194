{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Expr
import ExprT
import Parser

import qualified StackVM
import qualified VarExprT
import qualified Data.Map as M

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr ExprT where
  lit i   = Lit i
  add a b = Add a b
  mul a b = Mul a b

instance Expr Integer where
  lit = id
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit i
    | i <= 0 = False
    | otherwise = True
  add a b = a || b
  mul a b = a && b

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit = Mod7 . (flip mod 7)
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b

instance Expr StackVM.Program where
  lit i   = [StackVM.PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalWithVars :: VarExprT.VarExprT -> VarExprT.VarMap -> Maybe Integer
evalWithVars (VarExprT.Lit i) varmap   = Just i
evalWithVars (VarExprT.Add a b) varmap = do
  left  <- evalWithVars a varmap
  right <- evalWithVars b varmap
  return $ left + right
evalWithVars (VarExprT.Mul a b) varmap = do
  left  <- evalWithVars a varmap
  right <- evalWithVars b varmap
  return $ left + right
evalWithVars (VarExprT.Var x) varmap = M.lookup x varmap

evalStr :: String -> Maybe Integer
evalStr expr = parse expr >>= (return . eval)
  where
    parse :: String -> Maybe ExprT
    parse = parseExp Lit Add Mul

compile :: Expr a => String -> Maybe a
compile = parseExp lit add mul

testExp :: Expr a => Maybe a
testExp = compile "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7
testVarExprT = testExp :: Maybe VarExprT.VarExprT

withVars :: [(String, Integer)]
            -> (VarExprT.VarMap -> Maybe Integer)
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

