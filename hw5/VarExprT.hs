{-# LANGUAGE FlexibleInstances #-}

module VarExprT where

import Expr
import qualified Data.Map as M

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

type VarMap = M.Map String Integer

instance Expr (VarMap -> Maybe Integer) where
  lit = const . Just
  add lookupA lookupB = \varMap -> do
    a <- lookupA varMap
    b <- lookupB varMap
    return $ a + b
  mul lookupA lookupB = \varMap -> do
    a <- lookupA varMap
    b <- lookupB varMap
    return $ a * b

instance HasVars (VarMap -> Maybe Integer) where
  var = M.lookup

