module Database where
import Data.Map (Map, empty, findWithDefault, insertWith, toList)
import Parser (Rule)
import GHC.IO
import Data.HashSet
import Data.Hashable

data RuleSignature = RuleSignature {
  name :: String,
  arity :: Int
} deriving (Ord, Eq, Show)

data DbValue =
  DbRule Rule | DbBool Bool | DbCustomFunction String
  deriving (Eq, Show)
instance Hashable DbValue where hashWithSalt salt _d = salt

type Database = Map RuleSignature (HashSet DbValue)

createDb :: Database
createDb = Data.Map.empty

setDb :: RuleSignature -> DbValue -> Database -> Database
setDb rs dbv = insertWith union rs (fromList [dbv])

getDb :: RuleSignature -> Database -> HashSet DbValue
getDb = findWithDefault (fromList [DbBool False])

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

printEntry :: (RuleSignature, HashSet DbValue) -> [(RuleSignature, HashSet DbValue)] -> IO ()
printEntry (rs, vals) rest = do
  putStr (name rs ++ " (" ++ show (arity rs) ++ ") -> ")
  let valList = Data.HashSet.toList vals
  printValue (head valList)
  printValues (tail valList)
  printEntries rest

printEntries :: [(RuleSignature, HashSet DbValue)] -> IO ()
printEntries entries = case entries of
  [] -> putStr ""
  (current:rest) -> printEntry current rest

printDb :: Database -> IO ()
printDb = printEntries . Data.Map.toList
