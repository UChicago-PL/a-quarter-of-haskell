{- The basic functionality of a hypothetical RPN calculator with infinite stack -}

module Calc
    ( Calculation
    , kEnter
    , kAdd
    , kSub
    , kMul
    , kDiv
    , kSqrt
    , kSwap
    , kDup
    , perform
) where

import State

data InternalState = InternalState
    { stack :: [Double]
    , memory :: Double
    }

type CalcState = State InternalState

{- private functions -}

pop :: CalcState Double
pop = state $ \st -> case stack st of
    [] -> (0.0,st)
    x:xs -> (x,st { stack = xs })

push :: Double -> CalcState ()
push d = modify  $ \st -> st { stack = d : stack st }

unOp :: (Double -> Double) -> CalcState ()
unOp op = do
    x <- pop
    push $ op x

binOp :: (Double -> Double -> Double) -> CalcState ()
binOp op = do
    y <- pop
    x <- pop
    push $ op x y

{- exported types  -}

type Calculation = CalcState ()

{- exported calculations -}

kAdd, kSub, kMul, kDiv :: Calculation
kAdd = binOp (+)
kSub = binOp (-)
kMul = binOp (*)
kDiv = binOp (/)

kSqrt :: Calculation
kSqrt = unOp sqrt

kSin,kCos,kTan :: Calculation
kSin = unOp sin
kCos = unOp cos
kTan = unOp tan

{- exported stack operations -}

kEnter :: Double -> Calculation
kEnter = push

kSwap :: Calculation
kSwap = do
    y <- pop
    x <- pop
    push y
    push x

kDup :: Calculation
kDup = do
    x <- pop
    push x
    push x

{- execution of a calculator program -}

perform :: Calculation -> Double
perform ma = evalState (ma >> pop) startState where
    startState = InternalState { stack = [], memory = 0.0 }
