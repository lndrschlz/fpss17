-- author: leander schulz
-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visit where

import Data.Expr.Proposition.Types

-- ----------------------------------------

data Visitor r
  = V { vLit    :: Bool  -> r
      , vVar    :: Ident -> r
      , vUnary  :: Op1   -> r -> r
      , vBinary :: Op2   -> r -> r -> r
      }

idExpr :: Visitor Expr
idExpr
  = V { vLit = Lit, vVar = Var, vUnary = Unary, vBinary = Binary }

visit :: Visitor r -> Expr -> r
visit v (Lit b)	= vLit v b 
visit v (Var i) = vVar v i
visit v (Unary op u) = (vUnary v) op (visit v u)
visit v (Binary op e1 e2) = (vBinary v) op (visit v e1) (visit v e2)

--Expr:
--  Lit    Bool
--  Var    Ident
--  Unary  Op1 Expr
--  Binary Op2 Expr Expr
-- ----------------------------------------
