module Evaluate where

import qualified Data.Either as E
import qualified Data.Map as M
import qualified Definition as D
import qualified Expression as Expr
import qualified Statement as S
import qualified Value as V
import qualified Builtin as B

import Control.Monad
import Data.Maybe

data Error = LookUpError String                                      |
             AlreadyExistsError String                               |
             DoesNotExistError String                                |
             DivisionByZeroError                                     |
             ArgumentCountMismatchError                              |
             FunctionMustReturnValueError                            |
             BinaryOperatorError Expr.BinaryOperator V.Value V.Value |
             UnaryOperatorError Expr.UnaryOperator V.Value

instance Show Error where
    show (LookUpError name) = "Cannot read non-existent variable: " ++ name
    show (AlreadyExistsError name) = "Cannot introduce variable twice: " ++ name ++ " already defined"
    show (DoesNotExistError name) = "Cannot assign to non-existent variable: " ++ name
    show ArgumentCountMismatchError = "Wrong number of arguments provided"
    show FunctionMustReturnValueError = "Function must return value"
    show DivisionByZeroError = "Division by zero"
    show (BinaryOperatorError op left right) = "Cannot evaluate " ++ (show left) ++ " " ++ (show op) ++ " " ++ (show right)
    show (UnaryOperatorError op x) = "Cannot evaluate " ++ (show op) ++ (show x)

data Function = Function [String] [S.Statement]                          |
                BuiltinFunction0 V.Value                                 |
                BuiltinFunction1 (V.Value -> V.Value)                    |
                BuiltinFunction2 ((V.Value, V.Value) -> V.Value)         |
                BuiltinFunction3 ((V.Value, V.Value, V.Value) -> V.Value)

instance Show Function where
    show (Function argNames block) = "Function " ++ show argNames ++ " " ++ show block
    show (BuiltinFunction0 _) = "BuiltinFunction0"
    show (BuiltinFunction1 _) = "BuiltinFunction1"
    show (BuiltinFunction2 _) = "BuiltinFunction2"
    show (BuiltinFunction3 _) = "BuiltinFunction3"

data Procedure = Procedure [String] [S.Statement]
    deriving (Show)

data Context = GlobalContext (M.Map String V.Value) (M.Map String Function) (M.Map String Procedure) |
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
          impl (GlobalContext vtable _ _) = M.lookup name vtable
          impl (LocalContext parent nametable) = let result = M.lookup name nametable in
                                                 if isJust result
                                                 then result
                                                 else impl parent


introduceVariable :: String -> V.Value -> Evaluator ()
introduceVariable name x = do c <- getContext
                              case c of
                                  (GlobalContext vtable ftable ptable) -> if M.member name vtable
                                                                          then returnError (AlreadyExistsError name)
                                                                          else setContext (GlobalContext (M.insert name x vtable) ftable ptable)
                                  (LocalContext parent nametable) -> if M.member name nametable
                                                                     then returnError (AlreadyExistsError name)
                                                                     else setContext (LocalContext parent (M.insert name x nametable))

assignVariable :: String -> V.Value -> Evaluator ()
assignVariable name x = do c <- getContext
                           case (impl c) of
                               Just c' -> setContext c'
                               Nothing -> returnError (DoesNotExistError name)
    where impl :: Context -> Maybe Context
          impl (GlobalContext vtable ftable ptable) = if M.member name vtable
                                                      then Just (GlobalContext (M.insert name x vtable) ftable ptable)
                                                      else Nothing
          impl (LocalContext parent nametable) = if M.member name nametable
                                                 then Just (LocalContext parent (M.insert name x nametable))
                                                 else do parent' <- impl parent
                                                         return (LocalContext parent' nametable)

defineFunction :: String -> Function -> Evaluator ()
defineFunction name f = do c <- getContext
                           case (impl c) of
                               Just c' -> setContext c'
                               Nothing -> returnError (AlreadyExistsError name)
    where impl :: Context -> Maybe Context
          impl (GlobalContext vtable ftable ptable) = if M.member name ftable
                                                      then Nothing
                                                      else Just (GlobalContext vtable (M.insert name f ftable) ptable)
          impl (LocalContext parent vtable) = do parent' <- impl parent
                                                 return (LocalContext parent' vtable)

defineProcedure :: String -> Procedure -> Evaluator ()
defineProcedure name p = do c <- getContext
                            case (impl c) of
                                Just c' -> setContext c'
                                Nothing -> returnError (AlreadyExistsError name)
    where impl :: Context -> Maybe Context
          impl (GlobalContext vtable ftable ptable) = if M.member name ptable
                                                      then Nothing
                                                      else Just (GlobalContext vtable ftable (M.insert name p ptable))
          impl (LocalContext parent vtable) = do parent' <- impl parent
                                                 return (LocalContext parent' vtable)

getFunction :: String -> Evaluator Function
getFunction name = do c <- getContext
                      case (impl c) of
                          Just func -> return func
                          Nothing -> returnError (LookUpError name)
    where impl :: Context -> Maybe Function
          impl (GlobalContext _ ftable _) = M.lookup name ftable
          impl (LocalContext parent _) = impl parent

getProcedure :: String -> Evaluator Procedure
getProcedure name = do c <- getContext
                       case (impl c) of
                           Just proc -> return proc
                           Nothing -> returnError (LookUpError name)
    where impl :: Context -> Maybe Procedure
          impl (GlobalContext _ _ ptable) = M.lookup name ptable
          impl (LocalContext parent _) = impl parent

getGlobalContext :: Evaluator Context
getGlobalContext = do c <- getContext
                      return (strip c)
    where strip :: Context -> Context
          strip (LocalContext parent _) = parent
          strip gCtx = gCtx

setGlobalContext :: Context -> Evaluator ()
setGlobalContext gCtx = do c <- getContext
                           setContext (update c)
    where update :: Context -> Context
          update (LocalContext parent vtable) = LocalContext (update parent) vtable
          update _ = gCtx

introduceArgs :: [String] -> [V.Value] -> Evaluator ()
introduceArgs [] [] = return ()
introduceArgs (argName:argNames) (arg:args) = do introduceVariable argName arg
                                                 introduceArgs argNames args
introduceArgs _ _ = returnError ArgumentCountMismatchError

callProcedure :: Procedure -> [V.Value] -> Evaluator ()
callProcedure (Procedure argNames block) args = do ctx <- getContext
                                                   gCtx <- getGlobalContext
                                                   setContext gCtx
                                                   pushContext
                                                   introduceArgs argNames args
                                                   evaluateBlock block
                                                   popContext
                                                   gCtx' <- getGlobalContext
                                                   setContext ctx
                                                   setGlobalContext gCtx'

callFunction :: Function -> [V.Value] -> Evaluator V.Value
callFunction (Function argNames block) args = do ctx <- getContext
                                                 gCtx <- getGlobalContext
                                                 setContext gCtx
                                                 pushContext
                                                 introduceArgs argNames args
                                                 cf <- evaluateBlock block
                                                 popContext
                                                 gCtx' <- getGlobalContext
                                                 setContext ctx
                                                 setGlobalContext gCtx'
                                                 case cf of
                                                     Return x -> return x
                                                     _ -> returnError FunctionMustReturnValueError
callFunction (BuiltinFunction0 f) args = case args of
                                             [] -> return f
                                             _ -> returnError ArgumentCountMismatchError
callFunction (BuiltinFunction1 f) args = case args of
                                             [arg] -> return (f arg)
                                             _ -> returnError ArgumentCountMismatchError
callFunction (BuiltinFunction2 f) args = case args of
                                             [arg1, arg2] -> return (f (arg1, arg2))
                                             _ -> returnError ArgumentCountMismatchError
callFunction (BuiltinFunction3 f) args = case args of
                                             [arg1, arg2, arg3] -> return (f (arg1, arg2, arg3))
                                             _ -> returnError ArgumentCountMismatchError

emptyContext :: Context
emptyContext = GlobalContext M.empty M.empty M.empty

pushContext :: Evaluator ()
pushContext = do c <- getContext
                 setContext (LocalContext c M.empty)

popContext :: Evaluator ()
popContext = do c <- getContext
                case c of
                    (GlobalContext _ _ _) -> setContext emptyContext
                    (LocalContext parent _) -> setContext parent

applyBinaryOperator :: Expr.BinaryOperator -> V.Value -> V.Value -> E.Either Error V.Value
applyBinaryOperator Expr.Addition (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n + m))
applyBinaryOperator Expr.Subtraction (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n - m))
applyBinaryOperator Expr.Multiplication (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.IntegerValue (n * m))
applyBinaryOperator Expr.Division (V.IntegerValue n) (V.IntegerValue m) = if m == 0
                                                                          then E.Left DivisionByZeroError
                                                                          else E.Right (V.IntegerValue (div n m))
applyBinaryOperator Expr.Less (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.BooleanValue (n < m))
applyBinaryOperator Expr.Greater (V.IntegerValue n) (V.IntegerValue m) = E.Right (V.BooleanValue (n > m))
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
evaluateExpression (Expr.ExpressionCallFunction funcName argExprs) = do func <- getFunction funcName
                                                                        args <- mapM evaluateExpression argExprs
                                                                        callFunction func args

data ControlFlow = Finish | Return V.Value | ReturnNothing | Break | Continue
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
                                                                 ReturnNothing -> return ReturnNothing
                                                                 Break -> return Finish
                                                                 _ -> evaluateStatement (S.WhileStatement cond block)
                                                     else return Finish
evaluateStatement (S.ReturnStatement expr) = do value <- evaluateExpression expr
                                                return (Return value)
evaluateStatement (S.ReturnNothingStatement) = return ReturnNothing
evaluateStatement S.BreakStatement = return Break
evaluateStatement S.ContinueStatement = return Continue
evaluateStatement (S.IntroductionStatement name expr) = do value <- evaluateExpression expr
                                                           introduceVariable name value
                                                           return Finish
evaluateStatement (S.ProcedureCallStatement name argExprs) = do proc <- getProcedure name
                                                                args <- mapM evaluateExpression argExprs
                                                                callProcedure proc args
                                                                return Finish

evaluateBlock :: [S.Statement] -> Evaluator ControlFlow
evaluateBlock [] = return Finish
evaluateBlock (stmt:block) = do cf <- evaluateStatement stmt
                                case cf of
                                    Finish -> evaluateBlock block
                                    Return x -> return (Return x)
                                    ReturnNothing -> return ReturnNothing
                                    Break -> return Break
                                    Continue -> return Finish

evaluateScope :: [S.Statement] -> Evaluator ControlFlow
evaluateScope block = do pushContext
                         cf <- evaluateBlock block
                         popContext
                         return cf

evaluateDefinition :: D.Definition -> Evaluator ()
evaluateDefinition (D.GlobalVariableDefinition name expr) = do value <- evaluateExpression expr
                                                               introduceVariable name value
evaluateDefinition (D.FunctionDefinition name argNames block) = defineFunction name (Function argNames block)
evaluateDefinition (D.ProcedureDefinition name argNames block) = defineProcedure name (Procedure argNames block)

initBuiltin :: Evaluator ()
initBuiltin = do defineFunction "length" (BuiltinFunction1 B.builtinLength)
                 defineFunction "read" (BuiltinFunction2 B.builtinRead)
                 defineFunction "modify" (BuiltinFunction3 B.builtinModify)

evaluateProgram :: D.Program -> Evaluator ()
evaluateProgram program = do initBuiltin
                             mapM_ evaluateDefinition program
                             procMain <- getProcedure "main"
                             callProcedure procMain []
