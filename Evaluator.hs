module Evaluator where

import Database ( Database )
import Parser ( Program, Statement, printStatement )
import Text.Parsec (ParseError)
import System.Console.Terminfo (restoreDefaultColors)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

data Binding = Binding {
  name :: String,
  value :: String
} deriving (Eq, Show)

data Solution = Solution {
  statement :: Statement,
  results :: Either Bool [Binding]
} deriving (Eq, Show)

data Result =
    Error ParseError
  | Value [Solution]
  | Quit
  deriving (Eq, Show)

data EvalResult = EvalResult {
  db :: Database,
  result :: [Result]
} deriving (Eq, Show)

eval :: Database -> Statement -> EvalResult
eval db stmt = EvalResult db [Value [Solution stmt (Left True)]]

evalStep :: Database -> Statement -> [Statement] -> EvalResult
evalStep db_ current rest = do
  let currentResult = eval db_ current
  let restResult = iterateProgram (db currentResult) rest
  EvalResult (db restResult) (result currentResult ++ result restResult)

iterateProgram :: Database -> [Statement] -> EvalResult
iterateProgram db stmts = case stmts of
  []              -> EvalResult db []
  (current:rest)  -> evalStep db current rest

evalProlog :: Database -> Either ParseError Program -> [EvalResult]
evalProlog db ast = case ast of
  Left err    -> [EvalResult db [Error err]]
  Right prog  -> [EvalResult db []]

--Printer Helpers
printBindings :: [Binding] -> IO ()
printBindings [] = putStrLn "End Binding"
printBindings (current:rest) = do
  putStrLn "Binding"
  printBindings rest

printSolution :: Solution -> IO ()
printSolution sol = do
  printStatement (statement sol)
  hFlush stdout
  putStrLn ""
  case results sol of
    Left b        -> putStr (show b)
    Right binding -> printBindings binding

printSolutions :: [Solution] -> IO ()
printSolutions [] = putStr "False."
printSolutions (current:rest) = do
  printSolution current
  printSolutions rest

printResult :: [Result] -> IO ()
printResult [] = putStrLn "End Results"
printResult (current:rest) = do
  case current of
    Error e   -> print e
    Value val -> printSolutions val
    Quit      -> putStrLn "quit."
  printResult rest

printEvalResult :: EvalResult -> IO ()
printEvalResult evalResult = printResult (result evalResult)