module Database where
import Data.Map (Map, empty, findWithDefault, insertWith, toList)
import Parser
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

getDb :: RuleSignature -> Database -> [Term] -> HashSet DbValue
getDb rs db toMatch = Data.HashSet.filter (filterResults toMatch) (findWithDefault (fromList [DbBool False]) rs db)

matchArg :: Term -> Term -> Bool
matchArg toMatch arg = case toMatch of
  PTAtom matom -> case arg of
    PTAtom aatom -> atomname matom == atomname aatom
    PTVariable avar -> True
  PTVariable mvar -> True

matchArgList :: [Term] -> ArgList -> Bool
matchArgList [] [] = True
matchArgList [] something = False
matchArgList something [] = False
matchArgList (currentToMatch:restToMatch) (currentArg:restArgs) =
  case matchArg currentToMatch currentArg of
    True -> matchArgList restToMatch restArgs
    False -> False

filterResults :: [Term] -> DbValue -> Bool
filterResults toMatch dbValue = case dbValue of
  DbRule rule -> matchArgList toMatch (retrieveArgList (ruleHead rule))

retrieveArgList :: Compound -> ArgList
retrieveArgList comp = case comp of
  PCProFunctor func -> argList func
  PCProList list -> [] --I don't think this is right
  PCTerm term -> []

printValue :: DbValue -> IO ()
printValue val = case val of
    DbBool bv -> print bv
    DbRule r -> do
      printRule r
      putStrLn ""
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
