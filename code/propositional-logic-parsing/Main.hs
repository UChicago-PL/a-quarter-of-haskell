module Main where

import Control.Exception
import System.Environment
import System.IO
import Proposition

showArg :: String -> IO ()
showArg arg = do
    p <- try (readIO arg) :: IO (Either IOError Proposition)
    case p of
        Left _ -> putStrLn $ "Could not parse \'" ++ arg ++ "\'."
        Right r -> putStrLn $ show r

main :: IO ()
main = do
    args <- getArgs
    mapM_ showArg args
