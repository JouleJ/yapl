module Expression where

import qualified Parser as P
import qualified Fetch as F
import qualified Literal as L

data BinaryOperator = Addition | Subtraction | Multiplication | Division
    deriving (Show, Eq)

fetchBinaryOperator :: P.Parser BinaryOperator
fetchBinaryOperator = P.makeOr fetchAddition       $
                      P.makeOr fetchSubtraction    $
                      P.makeOr fetchMultiplication $
                      fetchDivision
    where fetchAddition = fmap (\_ -> Addition) $ F.fetch '+'
          fetchSubtraction = fmap (\_ -> Subtraction) $ F.fetch '-'
          fetchMultiplication = fmap (\_ -> Multiplication) $ F.fetch '*'
          fetchDivision = fmap (\_ -> Division) $ F.fetch '/'

data UnaryOperator = Negation
    deriving (Show, Eq)

fetchUnaryOperator :: P.Parser UnaryOperator
fetchUnaryOperator = fmap (\_ -> Negation) $ F.fetch '-'

binaryOperatorsListedByPriority :: [BinaryOperator]
binaryOperatorsListedByPriority = [Addition, Subtraction, Multiplication, Division]

data Expression = ExpressionVariable String                                      |
                  ExpressionLiteral L.Literal                                    |
                  ExpressionBinaryOperation BinaryOperator Expression Expression |
                  ExpressionUnaryOperation UnaryOperator Expression
    deriving (Show, Eq)

fetchVariableName :: P.Parser String
fetchVariableName = do x <- P.makeOr (F.fetch '_') F.fetchLetter
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

expressionTermParser :: P.Parser Expression
expressionTermParser = P.makeOr expressionVariableParser      $
                       P.makeOr expressionLiteralParser       $
                       P.makeOr expressionInParenthesisParser $
                       expressionUnaryOperationParser

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
