module Parser where

import Control.Monad

data Parser a = Parser (String -> [(a, String)])

runParser :: Parser a -> String -> [(a, String)]
runParser (Parser p) = p

combine :: Parser a -> (a -> Parser b) -> Parser b
combine (Parser p) f = Parser q
    where q s = concat $ map (\(x, y) -> runParser (f x) y) (p s)

instance Functor Parser where
    fmap f p = combine p (\x -> pure (f x))

instance Applicative Parser where
    pure x = Parser (\s -> [(x, s)])
    (<*>) p q = combine p (\f -> combine q (\x -> pure (f x)))

instance Monad Parser where
    (>>=) = combine
    return = pure
