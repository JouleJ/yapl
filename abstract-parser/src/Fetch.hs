module Fetch where

import Parser

fetch :: Char -> Parser Char
fetch c = Parser p
    where p :: String -> [(Char, String)]
          p [] = []
          p (x:xs) = if x == c then [(x, xs)] else []
