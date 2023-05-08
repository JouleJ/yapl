module Main where

import qualified Parser as P
import qualified Fetch as F

abcParser :: P.Parser ()
abcParser = do _ <- F.fetch 'a'
               _ <- F.fetch 'b'
               _ <- F.fetch 'c'
               return ()

test1 :: Bool
test1 = P.runParser abcParser "hello world" == []

test2 :: Bool
test2 = P.runParser abcParser "abcabc" == [((), "abc")]

test3 :: Bool
test3 = P.runParser (F.fetchString "hello") "hello world" == [("hello", " world")]

test4 :: Bool
test4 = P.runParser F.fetchBool "true" == [(True, "")]

test5 :: Bool
test5 = P.runParser F.fetchBool "false" == [(False, "")]

tests :: [Bool]
tests = [test1, test2, test3, test4, test5]

main = if all id tests then return () else error "Failure"
