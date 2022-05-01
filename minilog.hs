module Main where
import System.Directory (getCurrentDirectory)

import Parser ( parseProlog, createUnary, debugPrint )
import Database
    ( Database,
      createDb,
      RuleSignature(RuleSignature),
      DbValue(DbRule, DbCustomFunction, DbBool),
      setDb, printDb )
import Data.Functor;
import Evaluator ( Result(Quit), evalProlog, loadProgram, EvalResult(evalResults, resultDb), printEvalResult, printEvalResults )
import GHC.IO.Handle
import GHC.IO.Handle.FD
import System.IO
import System.Environment

initDb :: String -> Database
initDb cwd = 
  setDb (RuleSignature "true" 0)    (DbBool True) (
  setDb (RuleSignature "false" 0)   (DbBool False) (
  setDb (RuleSignature "quit" 0)    (DbCustomFunction "quit") (
  setDb (RuleSignature "cwd" 1)     (DbRule (createUnary "cwd" cwd)) (
  setDb (RuleSignature "write" 1)   (DbCustomFunction "write") (
  setDb (RuleSignature "nl" 0)      (DbCustomFunction "nl") (
  --setDb (RuleSignature "atom" 1)    (DbCustomFunction "atom") (
  --setDb (RuleSignature "number" 1)  (DbCustomFunction "number") (
  --setDb (RuleSignature "integer" 1) (DbCustomFunction "integer") (
  --setDb (RuleSignature "float" 1)   (DbCustomFunction "float") (
  --setDb (RuleSignature "negate" 1)  (DbCustomFunction "negate") (
  setDb (RuleSignature "load" 1)   (DbCustomFunction "load")
  createDb))))))--)))))

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

loadFile :: String -> Database -> IO Database
loadFile filepath db = do
  handle <- openFile filepath ReadMode
  contents <- hGetContents handle
  let parsed = parseProlog contents
  debugPrint parsed
  return (case parsed of
    Left error -> db -- TODO print errors here
    Right prog -> (loadProgram db prog))

loadFiles :: [String] -> Database -> IO Database
loadFiles [] db = do
  return db
loadFiles [current] db = do
  putStr "Loading: " 
  putStrLn current
  loadFile current db
loadFiles (current:rest) db = do
  putStr "Loading: "
  putStrLn current
  currentDb <- loadFile current db
  loadFiles rest currentDb


main :: IO ()
main = do
  args <- getArgs
  cwd <- getCurrentDirectory
  putStrLn "+========== minilog REPL ==========+"
  putStrLn "| Prolog implementation in Haskell |"
  putStrLn "+==================================+"
  db <- loadFiles args (initDb cwd)
  putStrLn "Current Database:"
  printDb db
  loop db


