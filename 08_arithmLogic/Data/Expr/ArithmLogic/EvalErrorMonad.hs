{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Expr.ArithmLogic.EvalErrorMonad where

import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Monad.Except

import Data.Expr.ArithmLogic.Types
import Data.Pretty

-- ----------------------------------------
--
-- simple expression evaluation with error handling
--
-- evaluates expressions without any free or bound variable

-- ----------------------------------------

data Value
  = B Bool
  | I Integer
    deriving (Eq, Ord, Show)
             
instance Pretty Value where
  pretty (B b) = pretty b
  pretty (I i) = pretty i

isB :: Value -> Bool
isB (B _) = True
isB _     = False

isI :: Value -> Bool
isI (I _) = True
isI _     = False

-- ----------------------------------------

data Result a
  = R { resVal :: a         }
  | E { resErr :: EvalError }
    deriving (Show)

instance Functor Result where
  fmap f c = c >>= return . f

instance Applicative Result where
  pure = return
  (<*>) = ap
  
instance Monad Result where
  return = R
  (R x) >>= f = f x
  (E x) >>= f = E x

instance MonadError EvalError Result where
-- throwError :: e -> m a
  throwError = E
-- catchError :: m a ->l (e -> m a) -> m a
  catchError (E e) f = f e
  catchError (R r) f = R r
  
instance (Pretty a) => Pretty (Result a) where
  pretty (R x) = pretty x
  pretty (E e) = "error: " ++ pretty e

-- ----------------------------------------
-- error handling
  
data EvalError
  = FreeVar String
  | NotImpl String
  | ValErr  String Value 
  | Div0
  | Mzero
  deriving (Show)

instance Pretty EvalError where
  pretty (FreeVar i)  = "free variable " ++ show i ++ " in expression"
  pretty (NotImpl n)  = n ++ " not implemented"
  pretty (ValErr e g) = e ++ " value expected, but got: " ++ pretty g
  pretty Div0         = "divide by zero"
  pretty Mzero        = "mzero"

boolExpected :: Value -> Result a
boolExpected = throwError . ValErr "Bool"

intExpected :: Value -> Result a
intExpected  = throwError . ValErr "Integer"

notImpl :: String -> Result a
notImpl = throwError . NotImpl

freeVar :: String -> Result a
freeVar = throwError . FreeVar

div0 :: Result a
div0  = throwError Div0

-- ----------------------------------------

eval :: Expr -> Result Value
eval (BLit b)          = R (B b)
eval (ILit i)          = R (I i)
eval (Var    x)        = freeVar ("eval (Var _)")
eval (Unary  op e1)    = do v1 <- eval e1 
                            mf1 op v1
eval (Binary op e1 e2) = do v1 <- eval e1 
                            v2 <- eval e2
                            mf2 op v1 v2
eval (Cond   c e1 e2)  = do b <- evalBool c
                            if b
                              then eval e1  
                              else eval e2
eval (Let _x _e1 _e2)  = notImpl "eval (Let _)"

evalBool :: Expr -> Result Bool
evalBool e
  = do r <- eval e
       case r of
        (B b) -> return b
        _     -> boolExpected r
  
-- ----------------------------------------
-- MF: Meaning function

type MF1 = Value -> Result Value

mf1 :: Op1 -> MF1
mf1 Not        = op1BB not
mf1 ToInt      = op1BI (toInteger . fromEnum)
mf1 UPlus      = op1II id
mf1 UMinus     = op1II (0 -)
mf1 Signum     = op1II signum
mf1 op         = \ _ -> notImpl "Operation"
  
op1BB :: (Bool -> Bool) -> MF1
op1BB op (B b) = return $ B (op b)
op1BB _  v     = boolExpected v

op1II :: (Integer -> Integer) -> MF1
op1II op (I i) = return (I (op i))
op1II _  v     = notImpl "Operation"

op1BI :: (Bool -> Integer) -> MF1
op1BI op (B b) = return (I (op b))
op1BI _  v     = notImpl "Operation"

-- ----------------------------------------

type MF2 = Value -> Value -> Result Value

mf2 :: Op2 -> MF2
mf2 And       = op2BBB (&&)
mf2 Or        = op2BBB (||)
mf2 Impl      = op2BBB (<=)
mf2 Xor       = op2BBB (/=)
mf2 Equiv     = op2BBB (==)
mf2 Plus      = op2III (+)
mf2 Minus     = op2III (-)
mf2 Mult      = op2III (*)
mf2 Div       = divIII div
mf2 Mod       = divIII mod
mf2 Eq        = op2IIB (==)
mf2 Neq       = op2IIB (/=)
mf2 Ge        = op2IIB (>=)
mf2 Gr        = op2IIB (>)
mf2 Le        = op2IIB (<=)
mf2 Ls        = op2IIB (<)
mf2 op        = \ _ _ -> notImpl (pretty op)

op2BBB :: (Bool -> Bool -> Bool) -> MF2
op2BBB op (B b1) (B b2) = return (B (b1 `op` b2))
op2BBB _  v1     v2     = notImpl "illegal operand with op :: Bool -> Bool -> Bool"

op2III :: (Integer -> Integer -> Integer) -> MF2
op2III op (I i1) (I i2) = return (I (i1 `op` i2))
op2III _  v1      v2    = notImpl "illegal operand with op :: Integer -> Integer -> Integer"

op2IIB :: (Integer -> Integer -> Bool) -> MF2
op2IIB op (I i1) (I i2) = return (B (i1 `op` i2))
op2IIB _  v1      v2    = notImpl "illegal operand with op :: Integer -> Integer -> Bool"


divIII :: (Integer -> Integer -> Integer) -> MF2
divIII op (I x) (I y)   = if y == 0
                            then div0
                            else return (I (x `op` y))
divIII _  v1      v2    = notImpl "illegal operand with op :: Integer -> Integer -> Integer"

-- ----------------------------------------
