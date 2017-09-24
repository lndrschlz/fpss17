-- author: leander schulz
module Data.Expr.Proposition.Substitute where

--freeVars: union excludes dublicates
import Data.List      (union)  

import Data.Expr.Proposition.Types

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env (Lit l) = Lit l
substVars env (Var v) =  snd . head . filter ((==v) . fst) $ env
substVars env (Unary op u) = Unary op (substVars env u)
substVars env (Binary op e1 e2) = Binary op (substVars env e1) (substVars env e2)

-- returns list of identiy elements of an expression
freeVars :: Expr -> [Ident]
freeVars (Lit _) = []
freeVars (Var v)        = [v]
freeVars (Unary _ u)    = freeVars u
freeVars (Binary _ e1 e2) = union (freeVars e1) (freeVars e2)

-- ----------------------------------------
