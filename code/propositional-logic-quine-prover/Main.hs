module Main where

import Control.Monad.State
import Data.Either
import System.Environment
import System.Exit
import System.IO
import Text.Read

import Proposition
import Quine
import QuineHtml

data TraversalState = TraversalState
    { index :: Int                 -- the index of the next proposition
    , tauts :: [(Int,Proposition)] -- processed, indexed tautologies
    }

startState :: TraversalState
startState = TraversalState { index = 1, tauts = [] }

inc :: State TraversalState ()
inc = modify $ \st -> st { index = index st + 1 }

nextIndex :: State TraversalState Int
nextIndex = gets index <* inc

processArg
    :: String
    -> State TraversalState ProofObject
processArg str = do
    ix <- nextIndex
    case readMaybe str of
        Nothing -> pure (ix,Left str)
        Just prop -> do
            ts <- gets tauts
            let analysis = analyze ts prop
            when (isRight analysis) $
                modify $ \st -> st{ tauts = (ix,prop):ts }
            pure (ix, Right (prop,analysis))            

usage :: IO ()
usage = do
    progname <- getProgName
    hPutStrLn stderr $ "usage: " ++ progname ++ " infile outfile"
    exitWith $ ExitFailure 255

main :: IO ()
main = do
    args <- getArgs
    case args of
        [infile,outfile] -> do
            objects <- lines <$> readFile infile
            writeFile outfile
                 . renderQuine
                 . flip evalState startState
                 . traverse processArg 
                 $ objects
        _ -> usage