module Parser where

import Control.Monad.State

type Parser = StateT String []

runParser :: Parser a -> String -> [(a,String)]
runParser = runStateT

parser :: (String -> [(a,String)]) -> Parser a
parser = StateT

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = parser $ \s -> case s of
    [] -> []
    a:as -> [(a,as) | f a]

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = mapM char
