module Evaluate where

import qualified Data.Either as E
import qualified Data.Map as M
import qualified Expression as Expr
import qualified Statement as S
import qualified Value as V

import Control.Monad
import Data.Maybe

data Error = LookUpError String                                      |
             AlreadyExistsError String                               |
             DoesNotExistError String                                |
             DivisionByZeroError                                     |
             BinaryOperatorError Expr.BinaryOperator V.Value V.Value |
             UnaryOperatorError Expr.UnaryOperator V.Value

instance Show Error where
    show (LookUpError name) = "Cannot read non-existent variable: " ++ name
    show (AlreadyExistsError name) = "Cannot introduce variable twice: " ++ name ++ " already defined"
    show (DoesNotExistError name) = "Cannot assign to non-existent variable: " ++ name
    show (DivisionByZeroError) = "Division by zero"
    show (BinaryOperatorError op left right) = "Cannot evaluate " ++ (show left) ++ " " ++ (show op) ++ " " ++ (show right)
    show (UnaryOperatorError op x) = "Cannot evaluate " ++ (show op) ++ (show x)

data Context = GlobalContext (M.Map String V.Value) |
               LocalContext Context (M.Map String V.Value)
    deriving (Show)

data Evaluator a = Evaluator (Context -> E.Either Error (a, Context))

runEvaluator :: Evaluator a -> Context -> E.Either Error (a, Context)
runEvaluator (Evaluator e) = e

combine :: Evaluator a -> (a -> Evaluator b) -> Evaluator b
combine (Evaluator e) f = Evaluator e'
    where e' c = case e c of
                    E.Right (x, c') -> runEvaluator (f x) c'
                    E.Left err -> E.Left err

instance Functor Evaluator where
    fmap f e = do x <- e
                  return (f x)

instance Applicative Evaluator where
    pure x = Evaluator (\c -> E.Right (x, c))
    (<*>) ef ex = do f <- ef
                     x <- ex
                     return (f x)

instance Monad Evaluator where
    (>>=) = combine

returnError :: Error -> Evaluator a
returnError err = Evaluator (\c -> E.Left err)

getContext :: Evaluator Context
getContext = Evaluator (\c -> E.Right (c, c))

setContext :: Context -> Evaluator ()
setContext c' = Evaluator (\c -> E.Right ((), c'))

readVariable :: String -> Evaluator V.Value
readVariable name = do c <- getContext
                       case (impl c) of
                           Just x -> return x
                           _ -> returnError (LookUpError name)
    where impl :: Context -> Maybe V.Value
          impl (GlobalContext nametable) = M.lookup name nametable
          impl (LocalContext parent nametable) = let result = M.lookup name nametable in
                                                 if isJust result
                                                 then result
                                                 else impl parent


introduceVariable :: String -> V.Value -> Evaluator ()
introduceVariable name x = do c <- getContext
                              case c of
                                  (GlobalContext nametable) -> if M.member name nametable
                                                               then returnError (AlreadyExistsError name)
                                                               else setContext (GlobalContext (M.insert name x nametable))
                                  (LocalContext parent nametable) -> if M.member name nametable
                                                                     then returnError (AlreadyExistsError name)
                                                                     else setContext (LocalContext parent (M.insert name x nametable))

assignVariable :: String -> V.Value -> Evaluator ()
assignVariable name x = do c <- getContext
                           case (impl c) of
                               Just c' -> setContext c'
                               Nothing -> returnError (DoesNotExistError name)
    where impl :: Context -> Maybe Context
          impl (GlobalContext nametable) = if M.member name nametable
                                           then Just (GlobalContext (M.insert name x nametable))
                                           else Nothing
          impl (LocalContext parent nametable) = if M.member name nametable
                                                 then Just (LocalContext parent (M.insert name x nametable))
                                                 else do parent' <- impl parent
                                                         return (LocalContext parent' nametable)

emptyContext :: Context
emptyContext = GlobalContext M.empty

pushContext :: Evaluator ()
pushContext = do c <- getContext
                 setContext (LocalContext c M.empty)

popContext :: Evaluator ()
popContext = do c <- getContext
                case c of
                    (GlobalContext _) -> setContext emptyContext
                    (LocalContext parent _) -> setContext parent

applyBinaryOperator :: Expr.BinaryOperator -> V.Value -> V.Value -> E.Either Error V.Value
applyBinaryOperator Expr.Addition (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n + m))
applyBinaryOperator Expr.Subtraction (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n - m))
applyBinaryOperator Expr.Multiplication (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n * m))
applyBinaryOperator Expr.Division (V.IntegerValue n) (V.IntegerValue m) = if m == 0
                                                                          then E.Left DivisionByZeroError
                                                                          else E.Right (V.IntegerValue (div n m))
applyBinaryOperator Expr.Less (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.BooleanValue (n < m))
applyBinaryOperator Expr.Concat (V.ListValue xs) (V.ListValue ys) = E.Right (V.ListValue (xs ++ ys))
applyBinaryOperator op left right = E.Left (BinaryOperatorError op left right)

applyUnaryOperator :: Expr.UnaryOperator -> V.Value -> E.Either Error V.Value
applyUnaryOperator Expr.Negation (V.IntegerValue n) = E.Right (V.IntegerValue (-n))
applyUnaryOperator op x = E.Left (UnaryOperatorError op x)

evaluateExpression :: Expr.Expression -> Evaluator V.Value
evaluateExpression (Expr.ExpressionVariable name) = readVariable name
evaluateExpression (Expr.ExpressionLiteral literal) = return (V.valueFromLiteral literal)
evaluateExpression (Expr.ExpressionBinaryOperation op leftExpr rightExpr) = do leftValue <- evaluateExpression leftExpr
                                                                               rightValue <- evaluateExpression rightExpr
                                                                               case applyBinaryOperator op leftValue rightValue of
                                                                                   E.Right x -> return x
                                                                                   E.Left err -> returnError err
evaluateExpression (Expr.ExpressionUnaryOperation op expr) = do value <- evaluateExpression expr
                                                                case applyUnaryOperator op value of
                                                                    E.Right x -> return x
                                                                    E.Left err -> returnError err
evaluateExpression (Expr.ExpressionList es) = fmap V.ListValue $ mapM evaluateExpression es

data ControlFlow = Finish | Return V.Value | Break | Continue
    deriving (Show)

evaluateStatement :: S.Statement -> Evaluator ControlFlow
evaluateStatement (S.AssignmentStatement name expr) = do value <- evaluateExpression expr
                                                         assignVariable name value
                                                         return Finish
evaluateStatement (S.IfStatement cond block) = do value <- evaluateExpression cond
                                                  if V.isTrue value
                                                  then evaluateScope block
                                                  else return Finish
evaluateStatement (S.IfElseStatement cond block block') = do value <- evaluateExpression cond
                                                             if V.isTrue value
                                                             then evaluateScope block
                                                             else evaluateScope block'
evaluateStatement (S.WhileStatement cond block) = do value <- evaluateExpression cond
                                                     if V.isTrue value
                                                     then do cf <- evaluateScope block
                                                             case cf of
                                                                 Return x -> return (Return x)
                                                                 Break -> return Finish
                                                                 _ -> evaluateStatement (S.WhileStatement cond block)
                                                     else return Finish
evaluateStatement (S.ReturnStatement expr) = do value <- evaluateExpression expr
                                                return (Return value)
evaluateStatement S.BreakStatement = return Break
evaluateStatement S.ContinueStatement = return Continue
evaluateStatement (S.IntroductionStatement name expr) = do value <- evaluateExpression expr
                                                           introduceVariable name value
                                                           return Finish

evaluateBlock :: [S.Statement] -> Evaluator ControlFlow
evaluateBlock [] = return Finish
evaluateBlock (stmt:block) = do cf <- evaluateStatement stmt
                                case cf of
                                    Finish -> evaluateBlock block
                                    Return x -> return (Return x)
                                    Break -> return Break
                                    Continue -> return Finish

evaluateScope :: [S.Statement] -> Evaluator ControlFlow
evaluateScope block = do pushContext
                         cf <- evaluateBlock block
                         popContext
                         return cf
