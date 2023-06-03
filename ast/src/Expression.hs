module Expression where

import qualified Parser as P
import qualified Fetch as F
import qualified Literal as L

data BinaryOperator = Addition       |
                      Subtraction    |
                      Multiplication |
                      Division       |
                      Less           |
                      Concat
    deriving (Show, Eq)

fetchBinaryOperator :: P.Parser BinaryOperator
fetchBinaryOperator = foldr1 P.makeOr [fetchAddition,
                                       fetchSubtraction,
                                       fetchMultiplication,
                                       fetchDivision,
                                       fetchLess,
                                       fetchConcat]
    where fetchAddition = fmap (\_ -> Addition) $ F.fetch '+'
          fetchSubtraction = fmap (\_ -> Subtraction) $ F.fetch '-'
          fetchMultiplication = fmap (\_ -> Multiplication) $ F.fetch '*'
          fetchDivision = fmap (\_ -> Division) $ F.fetch '/'
          fetchLess = fmap (\_ -> Less) $ F.fetch '<'
          fetchConcat = fmap (\_ -> Concat) $ F.fetchString "++"

data UnaryOperator = Negation
    deriving (Show, Eq)

fetchUnaryOperator :: P.Parser UnaryOperator
fetchUnaryOperator = fmap (\_ -> Negation) $ F.fetch '-'

binaryOperatorsListedByPriority :: [BinaryOperator]
binaryOperatorsListedByPriority = [Less,
                                   Addition,
                                   Concat,
                                   Subtraction,
                                   Multiplication,
                                   Division]

data Expression = ExpressionVariable String                                      |
                  ExpressionLiteral L.Literal                                    |
                  ExpressionBinaryOperation BinaryOperator Expression Expression |
                  ExpressionUnaryOperation UnaryOperator Expression              |
                  ExpressionList [Expression]
    deriving (Show, Eq)

keywords :: [String]
keywords = ["true",
            "false",
            "begin",
            "end",
            "do",
            "while",
            "if",
            "then",
            "else",
            "function",
            "procedure",
            "global"]

fetchVariableName :: P.Parser String
fetchVariableName = P.makeConditional (\x -> not $ elem x keywords) p
    where p :: P.Parser String
          p = do x <- P.makeOr (F.fetch '_') F.fetchLetter
                 xs <- P.makeList (P.makeOr (F.fetch '_') (P.makeOr F.fetchDigit F.fetchLetter))
                 return (x:xs)

expressionVariableParser :: P.Parser Expression
expressionVariableParser = fmap ExpressionVariable fetchVariableName

expressionLiteralParser :: P.Parser Expression
expressionLiteralParser = fmap ExpressionLiteral L.literalParser

expressionInParenthesisParser :: P.Parser Expression
expressionInParenthesisParser = do _ <- F.fetch '('
                                   _ <- F.skipWhitespace
                                   e <- expressionParser
                                   _ <- F.skipWhitespace
                                   _ <- F.fetch ')'
                                   return e

expressionUnaryOperationParser :: P.Parser Expression
expressionUnaryOperationParser = do u <- fetchUnaryOperator
                                    _ <- F.skipWhitespace
                                    e <- expressionParser
                                    return (ExpressionUnaryOperation u e)

expressionListParser :: P.Parser Expression
expressionListParser = do F.fetch '['
                          F.skipWhitespace
                          e <- expressionParser
                          es <- P.makeList auxillary
                          F.skipWhitespace
                          F.fetch ']'
                          return (ExpressionList (e:es))
    where auxillary :: P.Parser Expression
          auxillary = do F.skipWhitespace
                         F.fetch ','
                         F.skipWhitespace
                         e <- expressionParser
                         return e

expressionTermParser :: P.Parser Expression
expressionTermParser = foldr1 P.makeOr [expressionVariableParser,
                                        expressionLiteralParser,
                                        expressionInParenthesisParser,
                                        expressionUnaryOperationParser,
                                        expressionListParser]

expressionBinaryOperationParser :: P.Parser Expression
expressionBinaryOperationParser = helper binaryOperatorsListedByPriority expressionTermParser
    where helper [] p = p
          helper (x:xs) p = q
            where r = helper xs p
                  xr = do _ <- F.skipWhitespace
                          o <- P.makeConditional (== x) fetchBinaryOperator
                          _ <- F.skipWhitespace
                          e <- r
                          return (o, e)
                  xrList = P.makeList xr
                  q = do e <- r
                         oes <- xrList
                         return (buildExpression e (reverse oes))
                  buildExpression e [] = e
                  buildExpression e (oe:oes) = ExpressionBinaryOperation x (buildExpression e oes) (snd oe)

expressionParser :: P.Parser Expression
expressionParser = expressionBinaryOperationParser
