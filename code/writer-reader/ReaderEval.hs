module ReaderExpr where

import Reader

data UnaryOp
    = Abs
    | Signum
    deriving (Show)

data BinOp
    = Add | Subtract | Multiply
    deriving (Show)

data Expr n
    = Value n
    | ApplyBinary BinOp (Expr n) (Expr n)
    | ApplyUnary UnaryOp (Expr n)
    | Variable String
    | Let [(String,Expr n)] (Expr n)
    deriving (Show)

instance Num n => Num (Expr n) where
    (+) = ApplyBinary Add
    (-) = ApplyBinary Subtract
    (*) = ApplyBinary Multiply
    abs = ApplyUnary Abs
    signum = ApplyUnary Signum
    fromInteger = Value . fromInteger

type Eval n = Reader [(String,n)] n

-- | Look up a value in an association list, crashing if there is no such value.

lookup' :: String -> [(String,n)] -> n
lookup' var env = case lookup var env of
    Just val -> val
    Nothing -> error $ "Unknown variable " ++ var

-- | Evaluate an expression in the Eval monad.

eval :: (Num n) => Expr n -> Eval n
eval (Value n) = pure n
eval (ApplyBinary Add e1 e2) = applyBinary (+) e1 e2
eval (ApplyBinary Subtract e1 e2) = applyBinary (-) e1 e2
eval (ApplyBinary Multiply e1 e2) = applyBinary (*) e1 e2
eval (ApplyUnary Abs expr) = applyUnary abs expr
eval (ApplyUnary Signum expr) = applyUnary signum expr
eval (Variable v) = asks (lookup' v)
eval (Let bindings expr) = do
    let ks = map fst bindings
    vs <- mapM (eval . snd) bindings
    local (zip ks vs ++) $ eval expr

-- | Evaluate the result of applying a binary operator to an expression, in the
--   monad Eval.

applyBinary :: (Num n) => (n -> n -> n) -> Expr n -> Expr n -> Eval n
applyBinary op e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    let result = v1 `op` v2
    pure result

-- | Evaluate the result of applying a unary operator to an expression, in the
--   monad Eval.

applyUnary :: (Num n) => (n -> n) -> Expr n -> Eval n
applyUnary op expr = do
    v1 <- eval expr
    let result = op v1
    pure result

testExpr :: Expr Int
testExpr =
    let x = Variable "x"
        y = Variable "y"
    in Let [ ("y",10) ]
           $ Let [("x", y + y)]
                 (x * x - y * y)

test :: Int
test = runReader (eval testExpr) []

