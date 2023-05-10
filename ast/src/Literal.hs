module Literal (Literal, literalParser) where

import qualified Parser as P
import qualified Fetch as F

data Literal = IntegerLiteral Integer |
               BooleanLiteral Bool    |
               CharacterLiteral Char  |
               StringLiteral String
    deriving (Show, Eq)

integerLiteralParser :: P.Parser Literal
integerLiteralParser = fmap IntegerLiteral F.fetchInteger

booleanLiteralParser :: P.Parser Literal
booleanLiteralParser = fmap BooleanLiteral F.fetchBool

characterLiteralParser :: P.Parser Literal
characterLiteralParser = fmap CharacterLiteral F.fetchQuotedChar

stringLiteralParser :: P.Parser Literal
stringLiteralParser = fmap StringLiteral F.fetchQuotedString

literalParser :: P.Parser Literal
literalParser = P.makeOr integerLiteralParser   $
                P.makeOr booleanLiteralParser   $
                P.makeOr characterLiteralParser $
                stringLiteralParser
