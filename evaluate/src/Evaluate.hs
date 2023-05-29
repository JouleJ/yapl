module Evaluate where

import qualified Data.Either as E
import qualified Data.Map as M
import qualified Value as V
import qualified Expression as Expr

data Error = LookUpError String                                      |
             DivisionByZeroError                                     |
             BinaryOperatorError Expr.BinaryOperator V.Value V.Value |
             UnaryOperatorError Expr.UnaryOperator V.Value

instance Show Error where
    show (LookUpError name) = "No such variable: " ++ name
    show (DivisionByZeroError) = "Division by zero"
    show (BinaryOperatorError op left right) = "Cannot evaluate " ++ (show left) ++ " " ++ (show op) ++ " " ++ (show right)
    show (UnaryOperatorError op x) = "Cannot evaluate " ++ (show op) ++ (show x)

data Context = GlobalContext (M.Map String V.Value) |
               LocalContext Context (M.Map String V.Value)
    deriving (Show)

lookup :: String -> Context -> E.Either Error V.Value
lookup name (GlobalContext nametable) = case (M.lookup name nametable) of
                                            Just x -> E.Right x
                                            _ -> E.Left (LookUpError name)
lookup name (LocalContext parent nametable) = case (M.lookup name nametable) of
                                                Just x -> E.Right x
                                                _ -> Evaluate.lookup name parent

assign :: String -> V.Value -> Context -> Context
assign name x (GlobalContext nametable) = GlobalContext (M.insert name x nametable)
assign name x (LocalContext parent nametable) = if E.isRight (Evaluate.lookup name parent)
                                                then LocalContext (assign name x parent) nametable
                                                else LocalContext parent (M.insert name x nametable)

declare :: String -> V.Value -> Context -> Context 
declare name x (GlobalContext nametable) = GlobalContext (M.insert name x nametable)
declare name x (LocalContext parent nametable) = LocalContext parent (M.insert name x nametable)

emptyContext :: Context
emptyContext = GlobalContext M.empty

pushContext :: Context -> Context
pushContext context = LocalContext context M.empty

popContext :: Context -> Context
popContext (GlobalContext _) = emptyContext
popContext (LocalContext parent _) = parent

applyBinaryOperator :: Expr.BinaryOperator -> V.Value -> V.Value -> E.Either Error V.Value
applyBinaryOperator Expr.Addition (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n + m))
applyBinaryOperator Expr.Subtraction (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n - m))
applyBinaryOperator Expr.Multiplication (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n * m))
applyBinaryOperator Expr.Division (V.IntegerValue n) (V.IntegerValue m) = if m == 0
                                                                          then E.Left DivisionByZeroError
                                                                          else E.Right (V.IntegerValue (div n m))
applyBinaryOperator op left right = E.Left (BinaryOperatorError op left right)

applyUnaryOperator :: Expr.UnaryOperator -> V.Value -> E.Either Error V.Value
applyUnaryOperator Expr.Negation (V.IntegerValue n) = E.Right (V.IntegerValue (-n))
applyUnaryOperator op x = E.Left (UnaryOperatorError op x)

evaluateExpression :: Context -> Expr.Expression -> E.Either Error V.Value
evaluateExpression context (Expr.ExpressionVariable name) = Evaluate.lookup name context
evaluateExpression _ (Expr.ExpressionLiteral literal) = E.Right (V.valueFromLiteral literal)
evaluateExpression context (Expr.ExpressionBinaryOperation op left right) = do leftValue <- evaluateExpression context left
                                                                               rightValue <- evaluateExpression context right
                                                                               applyBinaryOperator op leftValue rightValue
evaluateExpression context (Expr.ExpressionUnaryOperation op e) = do value <- evaluateExpression context e
                                                                     applyUnaryOperator op value
