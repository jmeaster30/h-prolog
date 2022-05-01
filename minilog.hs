module Main where
import System.Directory (getCurrentDirectory)

import Parser ( parseProlog, createUnary )
import Database
    ( Database,
      createDb,
      RuleSignature(RuleSignature),
      DbValue(DbRule, DbCustomFunction, DbBool),
      setDb, printDb )
import Evaluator ( Result(Quit), evalProlog, EvalResult(evalResults, resultDb), printEvalResult, printEvalResults )
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

initDb :: String -> Database
initDb cwd = 
  setDb (RuleSignature "true" 0)    (DbBool True) (
  setDb (RuleSignature "false" 0)   (DbBool False) (
  setDb (RuleSignature "quit" 0)    (DbCustomFunction "quit") (
  setDb (RuleSignature "cwd" 1)     (DbRule (createUnary "cwd" cwd)) (
  setDb (RuleSignature "write" 1)   (DbCustomFunction "write") (
  setDb (RuleSignature "nl" 0)      (DbCustomFunction "nl") (
  setDb (RuleSignature "atom" 1)    (DbCustomFunction "atom") (
  --setDb (RuleSignature "number" 1)  (DbCustomFunction "number") (
  --setDb (RuleSignature "integer" 1) (DbCustomFunction "integer") (
  --setDb (RuleSignature "float" 1)   (DbCustomFunction "float") (
  --setDb (RuleSignature "negate" 1)  (DbCustomFunction "negate") (
  setDb (RuleSignature "load" 1)   (DbCustomFunction "load")
  createDb)))))))--))))

loop :: Database -> IO ()
loop db = do
  putStr "> "
  hFlush stdout
  line <- getLine
  let evalResult = evalProlog db (parseProlog line)
  let newDb = resultDb (last evalResult)
  printEvalResults evalResult
  case evalResults (last evalResult) of
    [Quit] -> putStrLn "Quitting minilog ..."
    _      -> loop newDb

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn "+========== minilog REPL ==========+"
  putStrLn "| Prolog implementation in Haskell |"
  putStrLn "+==================================+"
  --TODO load files and default facts here
  let db = initDb cwd
  putStrLn "Current Database:"
  printDb db
  loop db


