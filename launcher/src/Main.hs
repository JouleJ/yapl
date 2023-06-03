module Main where

import qualified Definition as D
import qualified Evaluate as E
import qualified Parser as P
import qualified PrettyPrint as PP

import Control.Monad
import Data.Either

parse :: String -> [D.Program]
parse = map fst . filter (null . snd) . P.runParser D.programParser

evaluate :: D.Program -> Either E.Error E.Context
evaluate program = fmap snd $ E.runEvaluator (E.evaluateProgram program) E.emptyContext

main :: IO ()
main = do possiblePrograms <- parse <$> getContents
          if null possiblePrograms
          then putStrLn "Failed to parse"
          else do if length possiblePrograms > 1
                  then do putStrLn "Warning: program can be parsed in more than one way"
                          forM (zip [0..] possiblePrograms) $ \(idx, program) -> do putStrLn $ "Program " ++ (show idx) ++ ": "
                                                                                    PP.prettyPrint 0 program
                          return ()
                  else return ()
                  case evaluate (head possiblePrograms) of
                      Right ctx -> PP.prettyPrint 0 ctx
                      Left err -> print err
