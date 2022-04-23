module Evaluator where

import Database ( Database, setDb, RuleSignature (RuleSignature), DbValue (DbRule), getDb )
import Parser ( Program, Statement (PSQuery, PSRule), printStatement, Query (queryBody), Rule (ruleHead, ruleBody), Compound (PCProFunctor, PCProList, PCTerm), Term (PTAtom, PTVariable), ProFunctor (funcName, argList), atomname, BinaryExpr, printRule, Atom (Atom), Variable (varname), ArgList )
import Text.Parsec (ParseError)
import System.Console.Terminfo (restoreDefaultColors)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import System.Posix.Internals (st_mtime)
import Data.HashSet
import Data.Map

data Binding = Binding {
  name :: String,
  value :: String
} deriving (Eq, Show)

data Solution = Solution {
  statement :: Statement,
  rule :: Maybe Rule,
  results :: Either Bool [Binding]
} deriving (Eq, Show)

data Result =
    Error (Either ParseError String)
  | Value [Solution]
  | Quit
  deriving (Eq, Show)

data EvalResult = EvalResult {
  resultDb :: Database,
  evalResults :: [Result]
} deriving (Eq, Show)

zipTerms :: Term -> Term -> (String, Term)
zipTerms a b = case a of
  PTAtom aatom -> case b of
    PTAtom batom -> if aatom == batom then (atomname aatom, PTAtom batom) else ("", PTAtom (Atom ""))
    PTVariable bvar -> (varname bvar, PTAtom aatom)
  PTVariable avar -> case b of
    PTAtom batom -> (varname avar, PTAtom batom)
    PTVariable bvar -> (varname avar, PTVariable bvar)

retrieveArgList :: Compound -> ArgList
retrieveArgList comp = case comp of
  PCProFunctor func -> argList func
  PCProList list -> [] --I don't think this is right
  PCTerm term -> []

bindVariables :: [Term] -> [Term] -> Map String Term
bindVariables ruleHead queryBody = Data.Map.fromAscList (zipWith zipTerms ruleHead queryBody)

createBinding :: (String, Term) -> Binding
createBinding (name, value) = case value of
  PTAtom atom -> Binding name (atomname atom)
  PTVariable var -> Binding name (varname var)

toBindings :: Map String Term -> [Binding]
toBindings = Prelude.map createBinding . Data.Map.toList
  
mapRule :: Database -> Statement -> Query -> DbValue -> EvalResult
mapRule db stmt query (DbRule rule) = do
  let boundVars = bindVariables (retrieveArgList (ruleHead rule)) (retrieveArgList (queryBody query))
  EvalResult db [Value [Solution stmt (Just rule) (Right (toBindings boundVars))]]
mapRule db stmt query other = EvalResult db [Value [Solution stmt Nothing (Left True)]]

evalQuery :: Database -> Statement -> Query -> [EvalResult]
evalQuery db stmt query = 
  case getFunctionSignature db stmt (queryBody query) of
    Left rulesig -> Prelude.map (mapRule db stmt query) (Data.HashSet.toList (getDb rulesig db))
    Right error -> [EvalResult db [Error (Right error)]]

validateExpr :: BinaryExpr -> Maybe String
validateExpr bexp = Nothing
--TODO I think we can use this in the future for validating rule bodies

getFunctionSignature :: Database -> Statement -> Compound -> Either RuleSignature String
getFunctionSignature db stmt head =
  case head of
    PCProFunctor func -> case funcName func of
        PTAtom atom -> Left (RuleSignature (atomname atom) (length (argList func)))
        PTVariable var -> Right "Rule head cannot be a variable"
    PCProList list -> Right "Rule head cannot be a list"
    PCTerm term -> case term of
      PTAtom atom -> Left (RuleSignature (atomname atom) 0)
      PTVariable var -> Right "Rule head cannot be a variable"


evalRule :: Database -> Statement -> Rule -> [EvalResult]
evalRule db stmt rule = do
  case getFunctionSignature db stmt (ruleHead rule) of
    Right error -> [EvalResult db [Error (Right error)]]
    Left rulesig -> [EvalResult (setDb rulesig (DbRule rule) db) [Value [Solution stmt Nothing (Left True)]]]

eval :: Database -> Statement -> [EvalResult]
eval db stmt = case stmt of
  PSQuery query -> evalQuery db stmt query
  PSRule rule -> evalRule db stmt rule

evalStep :: Database -> Statement -> [Statement] -> [EvalResult]
evalStep db_ current rest = do
  let currentResult = eval db_ current
  let restResult = iterateProgram (resultDb (head currentResult)) rest
  currentResult ++ restResult

iterateProgram :: Database -> [Statement] -> [EvalResult]
iterateProgram db stmts = case stmts of
  []              -> []
  (current:rest)  -> evalStep db current rest

evalProlog :: Database -> Either ParseError Program -> [EvalResult]
evalProlog db ast = case ast of
  Left err    -> [EvalResult db [Error (Left err)]]
  Right prog  -> iterateProgram db prog

--Printer Helpers
printBindings :: [Binding] -> IO ()
printBindings [] = putStrLn "End Binding"
printBindings (current:rest) = do
  putStrLn (name current ++ " = " ++ value current)
  printBindings rest

printSolution :: Solution -> IO ()
printSolution sol = do
  --putStrLn "statement :::::: "
  --printStatement (statement sol)
  --putStrLn "rule :::::::"
  case rule sol of
    Nothing -> putStr "nothing"
    Just x -> printRule x
  putStrLn ""
  --putStrLn "results ::::::"
  case results sol of
    Left b        -> putStr (show b)
    Right binding -> printBindings binding

printSolutions :: [Solution] -> IO ()
printSolutions [] = putStrLn "False."
printSolutions [current] = do
  printSolution current
  putStrLn ""
printSolutions (current:rest) = do
  printSolution current
  putStrLn ""
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
printEvalResult evalResult = do
  printResult (evalResults evalResult)

printEvalResults :: [EvalResult] -> IO ()
printEvalResults [] = putStrLn ""
printEvalResults (current:rest) = do
  printEvalResult current
  printEvalResults rest