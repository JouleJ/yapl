module Statement where

import qualified Parser as P
import qualified Fetch as F
import qualified Expression as E

data Statement = AssignmentStatement String E.Expression              |
                 IfStatement E.Expression [Statement]                 |
                 IfElseStatement E.Expression [Statement] [Statement] |
                 WhileStatement E.Expression [Statement]              |
                 ReturnStatement E.Expression                         |
                 ReturnNothingStatement                               |
                 BreakStatement                                       |
                 ContinueStatement                                    |
                 IntroductionStatement String E.Expression
    deriving (Show, Eq)

assignmentStatementParser :: P.Parser Statement
assignmentStatementParser = do name <- E.fetchVariableName
                               F.skipWhitespace
                               F.fetchString ":="
                               F.skipWhitespace
                               expr <- E.expressionParser
                               return (AssignmentStatement name expr)

ifStatementParser :: P.Parser Statement
ifStatementParser = do F.fetchString "if"
                       F.skipWhitespace
                       cond <- E.expressionParser
                       F.skipWhitespace
                       F.fetchString "then"
                       F.skipWhitespace
                       block <- blockParser
                       F.skipWhitespace
                       F.fetchString "end"
                       return (IfStatement cond block)

ifElseStatementParser :: P.Parser Statement
ifElseStatementParser = do F.fetchString "if"
                           F.skipWhitespace
                           cond <- E.expressionParser
                           F.skipWhitespace
                           F.fetchString "then"
                           F.skipWhitespace
                           block <- blockParser
                           F.skipWhitespace
                           F.fetchString "else"
                           block' <- blockParser
                           F.skipWhitespace
                           F.fetchString "end"
                           return (IfElseStatement cond block block')

whileStatementParser :: P.Parser Statement
whileStatementParser = do F.fetchString "while"
                          F.skipWhitespace
                          cond <- E.expressionParser
                          F.skipWhitespace
                          F.fetchString "do"
                          F.skipWhitespace
                          block <- blockParser
                          F.skipWhitespace
                          F.fetchString "end"
                          return (WhileStatement cond block)

returnStatementParser :: P.Parser Statement
returnStatementParser = do F.fetchString "return"
                           F.skipWhitespace
                           expr <- E.expressionParser
                           return (ReturnStatement expr)

returnNothingStatementParser :: P.Parser Statement
returnNothingStatementParser = do F.fetchString "return"
                                  return (ReturnNothingStatement)

breakStatementParser :: P.Parser Statement
breakStatementParser = do F.fetchString "break"
                          return BreakStatement

continueStatementParser :: P.Parser Statement
continueStatementParser = do F.fetchString "continue"
                             return ContinueStatement

introductionStatementParser :: P.Parser Statement
introductionStatementParser = do F.fetchString "var"
                                 F.skipWhitespace
                                 name <- E.fetchVariableName
                                 F.skipWhitespace
                                 F.fetchString ":="
                                 F.skipWhitespace
                                 expr <- E.expressionParser
                                 return (IntroductionStatement name expr)

statementParser :: P.Parser Statement
statementParser = foldr1 P.makeOr [assignmentStatementParser, 
                                   ifStatementParser,
                                   ifElseStatementParser,
                                   whileStatementParser,
                                   returnStatementParser,
                                   breakStatementParser,
                                   continueStatementParser,
                                   introductionStatementParser,
                                   returnNothingStatementParser]

blockParser :: P.Parser [Statement]
blockParser = P.makeList p
    where p :: P.Parser Statement
          p = do stmt <- statementParser
                 F.skipWhitespace
                 return stmt
