-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visitors where

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit
import Data.Expr.Proposition.Eval (mf1, mf2)

import Data.Set(Set)
import qualified Data.Set as S

-- ----------------------------------------

type Idents = Set Ident

freeVars :: Expr -> Idents
freeVars = visit freeVarsVisitor

-- visitor implementation for freeVars
freeVarsVisitor :: Visitor Idents
freeVarsVisitor = V {
	vLit = const S.empty, 
	vVar = S.singleton, 
	vUnary = (\ f -> id), 
	vBinary = (\ f -> S.union) }
    
type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env = visit (substVarsVisitor env)

-- visitor implementation for substVars
substVarsVisitor :: VarEnv -> Visitor Expr
substVarsVisitor env = V {
	vLit = Lit . id, 
	vVar = (\ i -> maybe (Var i) id (lookup i env)), 
	vUnary = Unary . id, 
	vBinary = Binary . id } 


eval :: Expr -> Bool
eval = visit evalVisitor

-- visitor implementation for eval
evalVisitor :: Visitor Bool
evalVisitor = V {
	vLit = id, 
	vVar = (\ x -> error "Found more variables than expected"), 
	vUnary = (\ f e -> (mf1 f) e), 
	vBinary = (\ f e1 e2 -> (mf2 f) e1 e2) }


-- ----------------------------------------
