module Quine where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup

import Proposition

data QuineProof
    = Split
        [Proposition]   -- ^ simplifications
        String          -- ^ variable to split on
        QuineProof      -- ^ case where split variable is true
        QuineProof      -- ^ case where split variable is false
    | Trivial
        [Proposition]   -- ^ simplifications
    | Reference
        [Proposition]   -- ^ simplifications
        Int             -- ^ index of reference tautology
        Proposition     -- ^ reference tautology
        Substitution    -- ^ reference map
    deriving (Show)

data Refutation = Refutation (Map String Bool)
    deriving (Show)

type Analysis = Either Refutation QuineProof

simplify :: Proposition -> Proposition
simplify = simpleEval Var Boolean notf andf orf impliesf where
    notf p = case p of
        Boolean b -> Boolean (not b)
        Not p' -> p'                    -- eliminate double negations
        _ -> Not p
    andf p q = case (p,q) of
        (Boolean True,_) -> q
        (Boolean False,_) -> Boolean False
        (_,Boolean True) -> p
        (_,Boolean False) -> Boolean False
        _ -> And p q
    orf p q = case (p,q) of
        (Boolean True,_) -> Boolean True
        (Boolean False,_) -> q
        (_,Boolean True) -> Boolean True
        (_,Boolean False) -> p
        _ -> Or p q
    impliesf p q = case (p,q) of
        (Boolean True,_) -> q
        (Boolean False,_) -> Boolean True
        (_,Boolean True) -> Boolean True
        (_,Boolean False) -> Not p
        _ -> Implies p q

simplifyInSteps :: Proposition -> [Proposition]
simplifyInSteps = converge step where
    converge f = untilFixed . iterate f where
        untilFixed (p:qs@(q:_))
            | p == q = [p]
            | otherwise = p : untilFixed qs
        untilFixed _ = error "untilFixed: impossible error"
    step prop = case prop of
        v@(Var _) -> v
        b@(Boolean _) -> b
        Not p -> case p of
            Boolean b -> Boolean (not b)
            Not p' -> p'
            _ -> Not $ step p
        And p q -> case (p,q) of
            (Boolean True,_) -> q
            (Boolean False,_) -> Boolean False
            (_,Boolean True) -> p
            (_,Boolean False) -> Boolean False
            _ -> And (step p) (step q)
        Or p q -> case (p,q) of
            (Boolean True,_) -> Boolean True
            (Boolean False,_) -> q
            (_,Boolean True) -> Boolean True
            (_,Boolean False) -> p
            _ -> Or (step p) (step q)
        Implies p q -> case (p,q) of
            (Boolean True,_) -> q
            (Boolean False,_) -> Boolean True
            (_,Boolean True) -> Boolean True
            (_,Boolean False) -> Not p
            _ -> Implies (step p) (step q)

instanceOf :: Proposition -> Proposition -> Maybe Substitution
instanceOf target pattern = case (target,pattern) of
    (t, Var v) -> Just $ Map.singleton v t
    (Boolean t,Boolean p) -> guard (t == p) >> pure Map.empty
    (Not t, Not p) -> instanceOf t p
    (And t1 t2, And p1 p2) -> process t1 t2 p1 p2
    (Or t1 t2, Or p1 p2) -> process t1 t2 p1 p2
    (Implies t1 t2, Implies p1 p2) -> process t1 t2 p1 p2
    _ -> Nothing
    where
        process t1 t2 p1 p2 = do
            m1 <- instanceOf t1 p1
            m2 <- instanceOf t2 p2
            if m1 `Map.intersection` m2 == m2 `Map.intersection` m1
                then pure $ Map.union m1 m2
                else Nothing

type Substitution = Map String Proposition

substitute :: Substitution -> Proposition -> Proposition
substitute sub = simpleEval varf Boolean Not And Or Implies where
    varf v = Map.findWithDefault (Var v) v sub

-- | A Heuristic selects the "attack variable" for a step of
--   Quine's algorithm. A Nothing result is returned if the
--   Proposition has no variables.

type Heuristic = Proposition -> Maybe String

-- | Produce a 'Map' that counts the number of times each item
--   occurs in a container.

counts :: (Foldable f,Ord a) => f a -> Map a Int
counts = fmap getSum . foldMap (`Map.singleton` (Sum 1))

-- | Determine the key for which a map takes on a maximal value.

argMax :: (Ord k, Ord a) => Map k a -> Maybe k
argMax m = fst <$> Map.foldrWithKey maxPair Nothing m where
    maxPair k1 a1 b2@(Just (_,a2)) =
        if a1 >= a2 then Just (k1,a1) else b2
    maxPair k1 a1 Nothing = Just (k1,a1)

variables :: Proposition -> Set String
variables = simpleEval Set.singleton (const Set.empty) id
                Set.union Set.union Set.union

heuristicFirst :: Heuristic
heuristicFirst = Set.lookupMin . variables

heuristicMost :: Heuristic
heuristicMost = argMax . counts . variables

analyzeWithHeuristic
    :: Heuristic              -- variable selection heuristic
    -> [(Int,Proposition)]    -- list of indexed tautologies
    -> Proposition            -- the Proposition to analyze
    -> Analysis
analyzeWithHeuristic heuristic tautologies = analyze_ Map.empty where
    analyze_ :: Map String Bool -> Proposition -> Analysis
    analyze_ valuation prop = case last steps of
        Boolean True -> pure $ Trivial steps
        Boolean False -> Left $ Refutation valuation
        prop' -> case findTaut prop' of
            Nothing -> do
                let var = fromJust $ heuristic prop'
                trueBranch  <- analyze_ (Map.insert var True valuation)
                                        (setVar prop' var True)
                falseBranch <- analyze_ (Map.insert var False valuation)
                                        (setVar prop' var False)
                pure $ Split steps var trueBranch falseBranch
            Just (i,pat,m) -> return $ Reference steps i pat m
        where
            steps = simplifyInSteps prop
    setVar p a b = substitute (Map.singleton a (Boolean b)) p
    findTaut target = msum $ map tryMatch tautologies where
        tryMatch (i,pat) = (,,) i pat <$> target `instanceOf` pat

analyze
    :: [(Int,Proposition)]
    -> Proposition
    -> Analysis
analyze = analyzeWithHeuristic heuristicMost
