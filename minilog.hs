module Main where
import System.Directory (getCurrentDirectory)

import Parser
import Database
import Evaluator
import Control.Monad
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

initDb :: Database
initDb = createDb

loop :: Database -> IO ()
loop db = do
  putStr "> "
  hFlush stdout
  line <- getLine
  let evalResult = evalProlog db (parseProlog line)
  print (result evalResult)
  case result evalResult of
    Quit x  -> putStrLn "Quitting minilog ..."
    _       -> loop db

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn "+========== minilog REPL ==========+"
  putStrLn "| Prolog implementation in Haskell |"
  putStrLn "+==================================+"
  --TODO load files and default facts here
  --TODO have database object that I can print here
  loop initDb


