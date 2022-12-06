-- | Define the 'Proposition' ADT for propositional boolean expressions,
--   including Show and Read instances, and an abstract evaluator.

module Proposition where

import Data.Char
import Data.Functor.Identity
import Text.ParserCombinators.ReadP as ReadP

-- | An abstract data type for representing boolean propositions.

data Proposition
    = Var String
    | Boolean Bool
    | Not Proposition
    | And Proposition Proposition
    | Or Proposition Proposition
    | Implies Proposition Proposition
    deriving Eq

-- | Strings that are used to define the syntactic representation of
--   various propositional operators.

impliesT,andT,orT,notT,trueT,falseT :: String
impliesT = "->"
andT = "&"
orT = "|"
notT = "!"
trueT = "T"
falseT = "F"

-- | A simple parser combinator that associates a string with a meaning

means :: String -> a -> ReadP a
name `means` meaning = skipSpaces *> ReadP.string name *> pure meaning

-- | A "missing" parser combinator for parsing zero or more applications
--   of a prefix operator to value.

prefix :: ReadP a -> ReadP (a -> a) -> ReadP a
prefix p op = result where
    result = p <++ (op <*> result)

-- | Modify a parser to expect to read parentheses around its input.

parens :: ReadP a -> ReadP a
parens = between (skipSpaces *> char '(') (skipSpaces *> char ')')

-- | The Read instance for Proposition.

instance Read Proposition where
    readsPrec _ = readP_to_S prec0 where
        prec0 = chainr1 prec1 (impliesT `means` Implies)
        prec1 = chainl1 prec2 (orT `means` Or)
        prec2 = chainl1 prec3 (andT  `means` And)
        prec3 = prefix  prec4 (notT `means` Not)
        prec4 = parseVar <++ parens prec0 <++ parseBool
        parseVar = skipSpaces
                 *> (Var . (:[]) <$> satisfy isLower)
        parseBool = Boolean <$> (trueT `means` True
                                <++ falseT `means` False)

-- | The Show instance for Proposition

instance Show Proposition where
    show = showp (0 :: Int) where
        showp _ (Boolean True) = trueT
        showp _ (Boolean False) = falseT
        showp _ (Var v) = v
        showp _ (Not p) = notT ++ showp 3 p
        showp i (And s t) =
            paren 2 i $ unwords [ showp 2 s, andT, showp 2 t ]
        showp i (Or s t)      =
            paren 1 i $ unwords [ showp 1 s, orT, showp 1 t ]
        showp i (Implies s t) =
            paren 0 i $ unwords [ showp 1 s, impliesT, showp 0 t]
        paren cutoff precedence str
            | precedence > cutoff = "(" ++ str ++ ")"
            | otherwise           = str

-- | An abstract evaluator for Propositions, in which evaluation takes
--   place within an evaluation context.

abstractEval :: (Applicative m)
             => (String -> m b)    -- ^ Var
             -> (Bool -> m b)      -- ^ Boolean
             -> (b -> b)         -- ^ Not
             -> (b -> b -> b)    -- ^ And
             -> (b -> b -> b)    -- ^ Or
             -> (b -> b -> b)    -- ^ Implies
             -> Proposition
             -> m b
abstractEval varf boolf notf andf orf impliesf = eval where
    eval (Var a)       = varf a
    eval (Boolean b)   = boolf b
    eval (Not p)       = pure notf  <*> eval p
    eval (And p q)     = pure andf <*> eval p <*> eval q
    eval (Or p q)      = pure orf  <*> eval p <*> eval q
    eval (Implies p q) = pure impliesf <*> eval p <*> eval q

-- | An evaluator for Propositions, in which evaluation
--   does not take place within an evaluation context.

simpleEval :: (String -> b)    -- ^ Var
           -> (Bool -> b)      -- ^ Boolean
           -> (b -> b)         -- ^ Not
           -> (b -> b -> b)    -- ^ And
           -> (b -> b -> b)    -- ^ Or
           -> (b -> b -> b)    -- ^ Implies
           -> Proposition
           -> b
simpleEval varf boolf notf andf orf impliesf prop
    = runIdentity
    $ abstractEval (Identity . varf)
                   (Identity . boolf)
                   notf andf orf impliesf prop

