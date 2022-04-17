module Database where
import Data.Map (Map, empty, findWithDefault, insertWith, toList)
import Parser (Rule)
import GHC.IO

data RuleSignature = RuleSignature {
  name :: String,
  arity :: Integer
} deriving (Ord, Eq, Show)

data DbValue =
  DbRule Rule | DbBool Bool | DbCustomFunction [Char]
  deriving (Eq, Show)

type Database = Map RuleSignature [DbValue]

createDb :: Database
createDb = empty

setDb :: RuleSignature -> DbValue -> Database ->  Database
setDb rs dbv = insertWith (++) rs [dbv]

getDb :: RuleSignature -> Database -> [DbValue]
getDb = findWithDefault [DbBool False]

printValue :: DbValue -> IO ()
printValue val = case val of
    DbBool bv               -> print bv
    DbRule r                -> print r
    DbCustomFunction fname  -> putStrLn ("Built-In '" ++ fname ++ "'") 

printCurrentValue :: DbValue -> [DbValue] -> IO ()
printCurrentValue current rest = do
  putStr      "\t| "
  printValue  current
  printValues rest

printValues :: [DbValue] -> IO ()
printValues values = case values of
  [] -> putStr ""
  (current:rest) -> printCurrentValue current rest

printEntry :: (RuleSignature, [DbValue]) -> [(RuleSignature, [DbValue])] -> IO ()
printEntry (rs, vals) rest = do
  putStr (name rs ++ " (" ++ show (arity rs) ++ ") -> ")
  printValue (head vals)
  printValues (tail vals)
  printEntries rest

printEntries :: [(RuleSignature, [DbValue])] -> IO ()
printEntries entries = case entries of
  [] -> putStr ""
  (current:rest) -> printEntry current rest

printDb :: Database -> IO ()
printDb = printEntries . toList
