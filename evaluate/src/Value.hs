module Value where

import qualified Literal as L

import Data.List (intercalate)

data Value = IntegerValue Integer |
             BooleanValue Bool    |
             CharacterValue Char  |
             StringValue String   |
             ListValue [Value]
    deriving (Eq)

instance Show Value where
    show (IntegerValue n) = show n
    show (BooleanValue b) = show b
    show (CharacterValue c) = show c
    show (StringValue s) = show s
    show (ListValue l) = "[" ++ (intercalate ", " (map show l)) ++ "]"

valueFromLiteral :: L.Literal -> Value
valueFromLiteral (L.IntegerLiteral x) = IntegerValue x
valueFromLiteral (L.BooleanLiteral x) = BooleanValue x
valueFromLiteral (L.CharacterLiteral x) = CharacterValue x
valueFromLiteral (L.StringLiteral x) = StringValue x
valueFromLiteral (L.ListLiteral xs) = ListValue (fmap valueFromLiteral xs)

isTrue :: Value -> Bool
isTrue (IntegerValue x) = x /= 0
isTrue (BooleanValue x) = x
isTrue (CharacterValue x) = x /= '\0'
isTrue (StringValue xs) = null xs
isTrue (ListValue xs) = null xs
