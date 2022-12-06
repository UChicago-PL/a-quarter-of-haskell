module Main where

import Calc

square :: Calculation
square = do
    kDup
    kMul

hypotenuse :: Calculation
hypotenuse = do
    square
    kSwap
    square
    kAdd
    kSqrt

test :: Double
test = perform $ do
    kEnter 1
    kEnter 2
    kAdd
    kEnter 3
    kMul    

main :: IO ()
main = print test
