module Fetch where

import Data.Char

import Parser

fetchAny :: Parser Char
fetchAny = Parser p
    where p :: String -> [(Char, String)]
          p [] = []
          p (x:xs) = [(x, xs)]

fetch :: Char -> Parser Char
fetch c = Parser p
    where p :: String -> [(Char, String)]
          p [] = []
          p (x:xs) = if x == c then [(x, xs)] else []

fetchWhitespace :: Parser Char
fetchWhitespace = makeConditional isSpace fetchAny

skipWhitespace :: Parser String
skipWhitespace = makeList fetchWhitespace 

fetchDigit :: Parser Char
fetchDigit = makeConditional isDigit fetchAny

fetchPositiveDigit :: Parser Char
fetchPositiveDigit = makeConditional (/= '0') fetchDigit

fetchString :: String -> Parser String
fetchString [] = return []
fetchString (x:xs) = do _ <- fetch x
                        _ <- fetchString xs
                        return (x:xs)

fetchBool :: Parser Bool
fetchBool = makeOr fetchFalse fetchTrue
    where fetchFalse :: Parser Bool
          fetchFalse = fetchString "false" >> return False
          fetchTrue :: Parser Bool
          fetchTrue = fetchString "true" >> return True

fetchPositiveInteger :: Parser Integer
fetchPositiveInteger = do first <- fetchPositiveDigit
                          tail <- makeList fetchDigit
                          return $ integerFromDigitList $ reverse $ first:tail
    where integerFromDigitList :: [Char] -> Integer
          integerFromDigitList [] = 0
          integerFromDigitList (x:xs) = (digitValue x) + 10 * (integerFromDigitList xs)
          digitValue :: Char -> Integer
          digitValue '0' = 0
          digitValue '1' = 1
          digitValue '2' = 2
          digitValue '3' = 3
          digitValue '4' = 4
          digitValue '5' = 5
          digitValue '6' = 6
          digitValue '7' = 7
          digitValue '8' = 8
          digitValue '9' = 9

fetchZero :: Parser Integer
fetchZero = fetch '0' >> return 0

fetchNegativeInteger :: Parser Integer
fetchNegativeInteger = do _ <- fetch '-'
                          n <- fetchPositiveInteger
                          return (-n)

fetchInteger :: Parser Integer
fetchInteger = makeOr fetchZero (makeOr fetchPositiveInteger fetchNegativeInteger)

fetchEscapedChar :: Parser Char
fetchEscapedChar = makeOr (fetchString "\\t" >> return '\t') $
                   makeOr (fetchString "\\n" >> return '\n') $
                   makeOr (fetchString "\\'" >> return '\'') $
                   makeOr (fetchString "\\\"" >> return '"') $
                   fetchString "\\r" >> return '\r'

fetchQuotedChar :: Parser Char
fetchQuotedChar = do _ <- fetch '\''
                     x <- makeOr (makeConditional (/= '\'') $ makeConditional (/= '\\') fetchAny) fetchEscapedChar
                     _ <- fetch '\''
                     return x

fetchQuotedString :: Parser String
fetchQuotedString = do _ <- fetch '"'
                       xs <- makeList $ makeOr (makeConditional (/= '"') $ makeConditional (/= '\\') fetchAny) fetchEscapedChar
                       _ <- fetch '"'
                       return xs
