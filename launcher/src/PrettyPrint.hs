module PrettyPrint where

import qualified Data.Map as M
import qualified Definition as D
import qualified Evaluate as Eval
import qualified Expression as E
import qualified Literal as L
import qualified Statement as S
import qualified Value as V

type Indent = Int

advance :: Indent -> Indent
advance = (+2)

putWithIndent :: Indent -> String -> IO ()
putWithIndent i s = putStrLn (take i (repeat ' ') ++ s)

printWithIndent :: Show a => Indent -> a -> IO ()
printWithIndent i = putWithIndent i . show

class PrettyPrint a where
    prettyPrint :: Indent -> a -> IO ()

instance PrettyPrint L.Literal where
    prettyPrint = printWithIndent

instance PrettyPrint E.BinaryOperator where
    prettyPrint = printWithIndent

instance PrettyPrint E.UnaryOperator where
    prettyPrint = printWithIndent

instance PrettyPrint E.Expression where
    prettyPrint i (E.ExpressionVariable name) = putWithIndent i name
    prettyPrint i (E.ExpressionLiteral literal) = prettyPrint i literal
    prettyPrint i (E.ExpressionBinaryOperation op leftExpr rightExpr) = do prettyPrint i op
                                                                           prettyPrint (advance i) leftExpr
                                                                           prettyPrint (advance i) rightExpr
    prettyPrint i (E.ExpressionUnaryOperation op expr) = do prettyPrint i op
                                                            prettyPrint (advance i) expr
    prettyPrint i (E.ExpressionList es) = do putWithIndent i "List"
                                             mapM_ (prettyPrint (advance i)) es

instance PrettyPrint S.Statement where
    prettyPrint i (S.AssignmentStatement name expr) = do putWithIndent i "Assignment"
                                                         putWithIndent (advance i) name
                                                         prettyPrint (advance i) expr
    prettyPrint i (S.IfStatement cond block) = do putWithIndent i "If"
                                                  prettyPrint (advance i) cond
                                                  putWithIndent i "Then"
                                                  prettyPrint (advance i) block
                                                  putWithIndent i "End"
    prettyPrint i (S.IfElseStatement cond block block') = do putWithIndent i "If"
                                                             prettyPrint (advance i) cond
                                                             putWithIndent i "Then"
                                                             prettyPrint (advance i) block
                                                             putWithIndent i "Else"
                                                             prettyPrint (advance i) block'
                                                             putWithIndent i "End"
    prettyPrint i (S.WhileStatement cond block) = do putWithIndent i "While"
                                                     prettyPrint (advance i) cond
                                                     putWithIndent i "Do"
                                                     prettyPrint (advance i) block
                                                     putWithIndent i "End"
    prettyPrint i (S.ReturnStatement expr) = do putWithIndent i "Return"
                                                prettyPrint (advance i) expr
    prettyPrint i S.ReturnNothingStatement = putWithIndent i "Return"
    prettyPrint i S.BreakStatement = putWithIndent i "Break"
    prettyPrint i S.ContinueStatement = putWithIndent i "Continue"
    prettyPrint i (S.IntroductionStatement name expr) = do putWithIndent i "Introduction"
                                                           putWithIndent (advance i) name
                                                           prettyPrint (advance i) expr
    prettyPrint i (S.ProcedureCallStatement name argExprs) = do putWithIndent i "CallProcedure"
                                                                putWithIndent (advance i) name
                                                                putWithIndent (advance i) "Arguments"
                                                                mapM_ (prettyPrint (advance . advance $ i)) argExprs

instance PrettyPrint [S.Statement] where
    prettyPrint i = mapM_ (prettyPrint i)

instance PrettyPrint D.Definition where
    prettyPrint i (D.GlobalVariableDefinition name expr) = do putWithIndent i "GlobalVariableDefintion"
                                                              putWithIndent (advance i) name
                                                              prettyPrint (advance i) expr
    prettyPrint i (D.FunctionDefinition name argNames block) = do putWithIndent i "FunctionDefinition" 
                                                                  putWithIndent (advance i) name
                                                                  putWithIndent (advance i) "Arguments"
                                                                  putWithIndent (advance i) "Body"
                                                                  prettyPrint (advance . advance $ i) block
    prettyPrint i (D.ProcedureDefinition name argNames block) = do putWithIndent i "ProcedureDefinition" 
                                                                   putWithIndent (advance i) name
                                                                   putWithIndent (advance i) "Arguments"
                                                                   mapM_ (putWithIndent (advance . advance $ i)) argNames
                                                                   putWithIndent (advance i) "Body"
                                                                   prettyPrint (advance . advance $ i) block

instance PrettyPrint [D.Definition] where
    prettyPrint i = mapM_ (prettyPrint i)

instance PrettyPrint V.Value where
    prettyPrint = printWithIndent

instance PrettyPrint Eval.Function where
    prettyPrint i (Eval.Function argNames block) = do putWithIndent i "Function"
                                                      putWithIndent (advance i) "Arguments"
                                                      mapM_ (putWithIndent (advance . advance $ i)) argNames
                                                      putWithIndent (advance i) "Body"
                                                      prettyPrint (advance . advance $ i) block

instance PrettyPrint Eval.Procedure where
    prettyPrint i (Eval.Procedure argNames block) = do putWithIndent i "Procedure"
                                                       putWithIndent (advance i) "Arguments"
                                                       mapM_ (putWithIndent (advance . advance $ i)) argNames
                                                       putWithIndent (advance i) "Body"
                                                       prettyPrint (advance . advance $ i) block

instance PrettyPrint a => PrettyPrint (M.Map String a) where
    prettyPrint i table = mapM_ handleEntry (M.toList table)
        where handleEntry (name, value) = do putWithIndent i name
                                             prettyPrint (advance i) value

instance PrettyPrint Eval.Context where
    prettyPrint i (Eval.GlobalContext vtable ftable ptable) = do putWithIndent i "GlobalContext"
                                                                 putWithIndent (advance i) "Variables"
                                                                 prettyPrint (advance . advance $ i) vtable
                                                                 putWithIndent (advance i) "Functions"
                                                                 prettyPrint (advance . advance $ i) ftable
                                                                 putWithIndent (advance i) "Procedures"
                                                                 prettyPrint (advance . advance $ i) ptable
    prettyPrint i (Eval.LocalContext parent vtable) = do putWithIndent i "LocalContext"
                                                         putWithIndent (advance i) "Variables"
                                                         prettyPrint (advance . advance $ i) vtable
                                                         putWithIndent (advance i) "Parent"
                                                         prettyPrint (advance . advance $ i) parent
