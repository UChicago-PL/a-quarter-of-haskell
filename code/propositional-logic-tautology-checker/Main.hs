module Main where

import Control.Monad ( void )
import Control.Monad.Reader
    ( runReader, MonadReader(reader) )
import Data.Functor.Identity ( Identity(Identity, runIdentity) )
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment ( getArgs )
import Text.Printf ( printf )
import Text.Read ( readMaybe )

import Proposition ( Proposition, abstractEval )

-- | The 'Set' of variables (as 'String') from a 'Proposition'.

variables :: Proposition -> Set String
variables = runIdentity . abstractEval
          (Identity . Set.singleton)     -- Var
          (const Set.empty)              -- Boolean
          id                             -- Not
          Set.union                      -- And
          Set.union                      -- Or
          Set.union                      -- Implies

-- | Given a 'Set' of names of variables, produce the
--   list of evaluations on those names.

valuations :: Set String -> [Map String Bool]
valuations = sequenceA . foldMap (`Map.singleton` [True,False])

-- | Evaluate a 'Proposition', given a valuation in the form of
--   'Map String Bool'. This code will throw an exception on an
--   unbound variable.

eval :: (Map String Bool)
     -> Proposition
     -> Bool
eval valuation prop = runReader (evalf prop) valuation where
    evalf = abstractEval (reader . flip (!))  -- Var
                         id                   -- Boolean
                         not                  -- Not
                         (&&)                 -- And
                         (||)                 -- Or
                         ((||) . not)         -- Implies

-- | The list of refuting valuations of a Proposition.

refutations :: Proposition -> [Map String Bool]
refutations p =
    [ v
    | v <- valuations . variables $ p
    , not . eval v $ p
    ]

-- | Interpret a 'String' as a 'Proposition', and analyze the
--   result for refutability, with an appropriate report going
--   to output.

process :: String -> IO ()
process arg =
    case readMaybe arg of
        Nothing -> putStrLn $ "Could not parse \'" ++ arg ++ "\'."
        Just p -> case refutations p of
            [] -> putStrLn $ show p ++ " is a tautology."
            r:_ -> do
                putStrLn $ show p ++ " is refutable, cf.,"
                void . (`Map.traverseWithKey` r) $ \k v ->
                    putStrLn $ printf "  %s := %s" k (show v)

-- | The main act

main :: IO ()
main = void $ getArgs >>= traverse process
