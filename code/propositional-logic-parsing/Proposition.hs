module Proposition (Proposition (..)) where

import Data.Char
import Data.Function
import Text.ParserCombinators.ReadP

data Proposition
    = Var String
    | Boolean Bool
    | Not Proposition
    | And Proposition Proposition
    | Or Proposition Proposition
    | Implies Proposition Proposition
    deriving (Eq)

impliesT,andT,orT,notT,trueT,falseT :: String
impliesT = "->"
andT = "&"
orT = "|"
notT = "!"
trueT = "T"
falseT = "F"

token :: a -> String -> ReadP a
token value str = skipSpaces *> string str *> pure value

prefix :: ReadP a -> ReadP (a -> a) -> ReadP a
prefix p op = fix $ (<++) p . (<*>) op

parens :: ReadP a -> ReadP a
parens = between (skipSpaces *> char '(') (skipSpaces *> char ')')

instance Read Proposition where
    readsPrec _ = readP_to_S prec0 where
        prec0 = chainr1 prec1 (token Implies impliesT)
        prec1 = chainl1 prec2 (token Or orT)
        prec2 = chainl1 prec3 (token And andT)
        prec3 = prefix  prec4 (token Not notT)
        prec4 = parseVar <++ parens prec0 <++ parseBool
        parseVar = skipSpaces
                 *> (Var <$> ((:) <$> satisfy isLower <*> munch isAlphaNum))
        parseBool =  Boolean <$> (token True trueT <++ token False falseT)

instance Show Proposition where
    show = prec 0 where
        prec _ (Boolean True) = trueT
        prec _ (Boolean False) = falseT
        prec _ (Var v) = v
        prec _ (Not p) = notT ++ prec 3 p
        prec i (And s t)     = paren 2 i $ unwords [ prec 2 s, andT, prec 2 t ]
        prec i (Or s t)      = paren 1 i $ unwords [ prec 1 s, orT, prec 1 t ]
        prec i (Implies s t) = paren 0 i $ unwords [ prec 1 s, impliesT, prec 0 t]
        paren cutoff prec str
            | prec > cutoff = "(" ++ str ++ ")"
            | otherwise     = str
