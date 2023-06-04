module Definition where

import qualified Expression as E
import qualified Fetch as F
import qualified Parser as P
import qualified Statement as S

data Definition = GlobalVariableDefinition String E.Expression     |
                  FunctionDefinition String [String] [S.Statement] |
                  ProcedureDefinition String [String] [S.Statement]
    deriving (Show)

globalVariableDefinitionParser :: P.Parser Definition
globalVariableDefinitionParser = do F.fetchString "global"
                                    F.skipWhitespace
                                    name <- E.fetchVariableName
                                    F.skipWhitespace
                                    F.fetchString ":="
                                    F.skipWhitespace
                                    expr <- E.expressionParser
                                    return (GlobalVariableDefinition name expr)

functionArgumentsParser :: P.Parser [String]
functionArgumentsParser = do F.fetch '('
                             F.skipWhitespace
                             argNames <- P.makeOr p (return [])
                             F.skipWhitespace
                             F.fetch ')'
                             return argNames
    where p, q :: P.Parser [String]
          p = do argName <- E.fetchVariableName
                 F.skipWhitespace
                 tail <- P.makeOr q (return [])
                 return (argName:tail)
          q = do F.fetch ','
                 F.skipWhitespace
                 p

functionDefinitionParser :: P.Parser Definition
functionDefinitionParser = do F.fetchString "function"
                              F.skipWhitespace
                              fName <- E.fetchVariableName
                              F.skipWhitespace
                              argNames <- functionArgumentsParser
                              F.skipWhitespace
                              F.fetchString "begin"
                              F.skipWhitespace
                              body <- S.blockParser
                              F.skipWhitespace
                              F.fetchString "end"
                              return (FunctionDefinition fName argNames body)

procedureDefinitionParser :: P.Parser Definition
procedureDefinitionParser = do F.fetchString "procedure"
                               F.skipWhitespace
                               pName <- E.fetchVariableName
                               F.skipWhitespace
                               argNames <- functionArgumentsParser
                               F.skipWhitespace
                               F.fetchString "begin"
                               F.skipWhitespace
                               body <- S.blockParser
                               F.skipWhitespace
                               F.fetchString "end"
                               return (ProcedureDefinition pName argNames body)

definitionParser :: P.Parser Definition
definitionParser = foldr1 P.makeOr [globalVariableDefinitionParser,
                                    functionDefinitionParser,
                                    procedureDefinitionParser]

type Program = [Definition]

programParser :: P.Parser Program
programParser = do F.skipWhitespace
                   P.makeList p
    where p :: P.Parser Definition
          p = do def <- definitionParser
                 F.skipWhitespace
                 return def
