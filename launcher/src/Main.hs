module Main where

import qualified Evaluate as E
import qualified Parser as P
import qualified Statement as S

import Control.Monad
import Data.List
import Data.Either

type Program = [S.Statement]

countNodes :: Program -> Integer
countNodes block = sum $ map S.countNodes block

parse :: String -> [Program]
parse = sortBy (\x y -> compare (countNodes x) (countNodes y)) . map fst . filter (null . snd) . P.runParser S.blockParser

evaluate :: Program -> Either E.Error (E.ControlFlow, E.Context)
evaluate block = E.runEvaluator (E.evaluateBlock block) E.emptyContext

main :: IO ()
main = do possiblePrograms <- parse <$> getContents
          if null possiblePrograms
          then putStrLn "Failed to parse"
          else do if length possiblePrograms > 1
                  then putStrLn "Warning: program can be parsed in more than one way"
                  else return ()
                  print (evaluate (head possiblePrograms))
