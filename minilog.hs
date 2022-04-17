module Main where
import System.Directory (getCurrentDirectory)

import Parser ( parseProlog, createUnary )
import Database
    ( Database,
      createDb,
      RuleSignature(RuleSignature),
      DbValue(DbRule, DbCustomFunction, DbBool),
      setDb, printDb )
import Evaluator ( Result(Quit), evalProlog, EvalResult(result), printEvalResult )
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

initDb :: [Char] -> Database
initDb cwd = 
  setDb (RuleSignature "true" 0)    (DbBool True) (
  setDb (RuleSignature "false" 0)   (DbBool False) (
  setDb (RuleSignature "quit" 0)    (DbCustomFunction "quit") (
  setDb (RuleSignature "cwd" 1)     (DbRule (createUnary "cwd" cwd)) (
  setDb (RuleSignature "write" 1)   (DbCustomFunction "write") (
  setDb (RuleSignature "nl" 0)      (DbCustomFunction "nl") (
  setDb (RuleSignature "atom" 1)    (DbCustomFunction "atom") (
  setDb (RuleSignature "number" 1)  (DbCustomFunction "number") (
  setDb (RuleSignature "integer" 1) (DbCustomFunction "integer") (
  setDb (RuleSignature "float" 1)   (DbCustomFunction "float") (
  setDb (RuleSignature "negate" 1)  (DbCustomFunction "negate")
  createDb))))))))))

loop :: Database -> IO ()
loop db = do
  putStr "> "
  hFlush stdout
  line <- getLine
  let evalResult = evalProlog db (parseProlog line)
  printEvalResult (last evalResult)
  case result (last evalResult) of
    [Quit] -> putStrLn "Quitting minilog ..."
    _      -> loop db

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn "+========== minilog REPL ==========+"
  putStrLn "| Prolog implementation in Haskell |"
  putStrLn "+==================================+"
  --TODO load files and default facts here
  --TODO have database object that I can print here
  let db = initDb cwd
  putStrLn "Current Database:"
  printDb db
  loop db


