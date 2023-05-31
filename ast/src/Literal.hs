module Literal (Literal (IntegerLiteral
                        , BooleanLiteral
                        , CharacterLiteral
                        , StringLiteral
                        , ListLiteral)
               , literalParser) where

import qualified Parser as P
import qualified Fetch as F

data Literal = IntegerLiteral Integer |
               BooleanLiteral Bool    |
               CharacterLiteral Char  |
               StringLiteral String   |
               ListLiteral [Literal]
    deriving (Show, Eq)

integerLiteralParser :: P.Parser Literal
integerLiteralParser = fmap IntegerLiteral F.fetchInteger

booleanLiteralParser :: P.Parser Literal
booleanLiteralParser = fmap BooleanLiteral F.fetchBool

characterLiteralParser :: P.Parser Literal
characterLiteralParser = fmap CharacterLiteral F.fetchQuotedChar

stringLiteralParser :: P.Parser Literal
stringLiteralParser = fmap StringLiteral F.fetchQuotedString

emptyListLiteralParser :: P.Parser Literal
emptyListLiteralParser = do F.fetchString "[]"
                            return (ListLiteral [])

listLiteralParser :: P.Parser Literal
listLiteralParser = do _ <- F.fetch '['
                       _ <- F.skipWhitespace
                       x <- literalParser
                       xs <- P.makeList auxillary
                       _ <- F.skipWhitespace
                       _ <- F.fetch ']'
                       return (ListLiteral (x:xs))
    where auxillary :: P.Parser Literal
          auxillary = do _ <- F.skipWhitespace
                         _ <- F.fetch ','
                         _ <- F.skipWhitespace
                         x <- literalParser
                         return x

literalParser :: P.Parser Literal
literalParser = foldr1 P.makeOr [integerLiteralParser,
                                 booleanLiteralParser,
                                 characterLiteralParser,
                                 stringLiteralParser,
                                 listLiteralParser,
                                 emptyListLiteralParser]
