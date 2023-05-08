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

tests :: [Bool]
tests = [test1, test2]

main = if all id tests then return () else error "Failure"
