module WriterEval where

import Control.Monad.Writer

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

instance Num n => Num (Expr n) where
    (+) = ApplyBinary Add
    (-) = ApplyBinary Subtract
    (*) = ApplyBinary Multiply
    abs = ApplyUnary Abs
    signum = ApplyUnary Signum
    fromInteger = Value . fromInteger

type Eval = Writer [(String,String)]

-- | Log a message that an operation was performed.

note :: String -> [String] -> Eval ()
note op ws = tell [(op,unwords ws)]

-- | Evaluate an expression in the Eval monad.

eval :: (Num n,Show n) => Expr n -> Eval n
eval (Value n) = pure n
eval (ApplyBinary Add e1 e2) = applyBinary "+" (+) e1 e2
eval (ApplyBinary Subtract e1 e2) = applyBinary "-" (-) e1 e2
eval (ApplyBinary Multiply e1 e2) = applyBinary "*" (*) e1 e2
eval (ApplyUnary Abs expr) = applyUnary "abs" abs expr
eval (ApplyUnary Signum expr) = applyUnary "signum" signum expr

-- | Evaluate the result of applying a binary operator to an expression, in the
--   monad Eval.

applyBinary :: (Num n,Show n) => String -> (n -> n -> n) -> Expr n -> Expr n -> Eval n
applyBinary name op e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    let result = v1 `op` v2
    note name [show v1,name,show v2,"=",show result]
    pure result

-- | Evaluate the result of applying a unary operator to an expression, in the
--   monad Eval.

applyUnary :: (Num n,Show n) => String -> (n -> n) -> Expr n -> Eval n
applyUnary name op expr = do
    v1 <- eval expr
    let result = op v1
    note name [name,show v1,"=",show result]
    pure result

showWork :: (Num n,Show n) => Expr n -> IO ()
showWork expr = do
    let (result,output) = runWriter . eval $ expr
    putStr . unlines . map snd $ output
    putStrLn $ "The final answer is " ++ show result ++ "."

showHardWork :: (Num n,Show n) => Expr n -> IO ()
showHardWork expr = do
    let (result,output) = runWriter . censor (filter isHard) . eval $ expr
    putStr . unlines . map snd $ output
    putStrLn $ "The final answer is " ++ show result ++ "."
    where
        isHard (op,_) = op == "*"

expr :: Expr Int
expr = (2 * 3) + (4 * (5 + 6))

