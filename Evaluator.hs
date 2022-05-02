module Evaluator where

import Database
import Parser
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

zipQueryTerms :: Term -> Term -> (String, Term)
zipQueryTerms a b = case a of
  PTAtom aatom -> case b of
    PTAtom batom -> ("", PTAtom (Atom ""))
    PTVariable bvar -> ("", PTAtom (Atom ""))
  PTVariable avar -> case b of
    PTAtom batom -> (varname avar, PTAtom batom)
    PTVariable bvar -> (varname avar, PTVariable bvar)

zipTerms :: Term -> Term -> (String, Term)
zipTerms a b = case a of
  PTAtom aatom -> case b of
    PTAtom batom -> if atomname aatom == atomname batom then (atomname aatom, PTAtom batom) else ("", PTAtom (Atom ""))
    PTVariable bvar -> ("", PTAtom (Atom ""))
  PTVariable avar -> case b of
    PTAtom batom -> (varname avar, PTAtom batom)
    PTVariable bvar -> ("", PTAtom (Atom ""))

filterEmptyTerms :: (String, Term) -> Bool
filterEmptyTerms (first, second) = case first of
  [] -> False
  other -> True

bindVariables :: [Term] -> [Term] -> Map String Term
bindVariables ruleHead queryBody = Data.Map.fromAscList (Prelude.filter (filterEmptyTerms) (zipWith zipTerms ruleHead queryBody))

bindQueryVariables :: [Term] -> [Term] -> Map String Term
bindQueryVariables ruleHead queryBody = Data.Map.fromAscList (Prelude.filter (filterEmptyTerms) (zipWith zipQueryTerms ruleHead queryBody))

createBinding :: (String, Term) -> Binding
createBinding (name, value) = case value of
  PTAtom atom -> Binding name (atomname atom)
  PTVariable var -> Binding name (varname var)

toBindings :: Map String Term -> [Binding]
toBindings = Prelude.map createBinding . Data.Map.toList

resolveBinding :: Map String Term -> Term -> Term
resolveBinding resolver value = case value of
  PTAtom atom -> value
  PTVariable var -> findWithDefault (PTAtom (Atom "_")) (varname var) resolver -- Is this default correct?

resolveBindings :: Map String Term -> Map String Term -> Map String Term
resolveBindings orig resolver = Data.Map.map (resolveBinding resolver) orig

mapRule :: Database -> Statement -> Query -> DbValue -> EvalResult
mapRule db stmt query (DbRule rule) = do
  let queryVars = bindQueryVariables (retrieveArgList (queryBody query)) (retrieveArgList (ruleHead rule))
  let boundVars = bindVariables (retrieveArgList (ruleHead rule)) (retrieveArgList (queryBody query))
  EvalResult db [Value (buildResults stmt rule queryVars (evalExpr db (apply (ruleBody rule) boundVars) boundVars))]
mapRule db stmt query other = EvalResult db [Value [Solution stmt Nothing (Left False)]] --TODO need to fix this for the other db value options

buildResults :: Statement -> Rule -> Map String Term -> [Map String Term] -> [Solution]
buildResults stmt rule queryVars [] = [Solution stmt (Just rule) (Left False)]
buildResults stmt rule queryVars [currentBinding] = [Solution stmt (Just rule) (Right (toBindings (resolveBindings queryVars currentBinding)))]
buildResults stmt rule queryVars (currentBinding:restBinding) = 
  [Solution stmt (Just rule) (Right (toBindings (resolveBindings queryVars currentBinding)))] ++
  (buildResults stmt rule queryVars restBinding)

applyTerm :: Map String Term -> Term -> Term
applyTerm bindings term = case term of
  PTAtom atom -> PTAtom atom
  PTVariable var -> findWithDefault (PTVariable var) (varname var) bindings

applyList :: ProList -> Map String Term -> ProList
applyList list bindings = case list of
  PLString string -> PLString string
  PLTerms  terms  -> PLTerms (Prelude.map (applyTerm bindings) terms)

applyFunctor :: ProFunctor -> Map String Term -> ProFunctor
applyFunctor func bindings = ProFunctor (funcName func) (Prelude.map (applyTerm bindings) (argList func))

applyCompound :: Compound -> Map String Term -> Compound
applyCompound comp bindings = case comp of
  PCProFunctor func -> PCProFunctor (applyFunctor func bindings)
  PCProList    list -> PCProList (applyList list bindings)
  PCTerm       term -> PCTerm (applyTerm bindings term)

apply :: BinaryExpr -> Map String Term -> BinaryExpr
apply expr bindings = case expr of
  BEConjunct (left, right) -> BEConjunct ((applyCompound left bindings), (apply right bindings))
  BEDisjunct (left, right) -> BEDisjunct ((applyCompound left bindings), (apply right bindings))
  BEPrimary  prim          -> BEPrimary  (applyCompound prim bindings) 

evalExpr :: Database -> BinaryExpr -> Map String Term -> [Map String Term]
evalExpr db expr bindings = 
  case expr of
    BEConjunct (lhs, rhs) -> [Data.Map.empty]
    BEDisjunct (lhs, rhs) -> [Data.Map.empty]
    BEPrimary  prim       -> [Data.Map.empty]

evalQuery :: Database -> Statement -> Query -> [EvalResult]
evalQuery db stmt query = 
  case getFunctionSignature db stmt (queryBody query) of
    Left rulesig -> Prelude.map (mapRule db stmt query) (Data.HashSet.toList (getDb rulesig db (retrieveArgList (queryBody query))))
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
evalStep db current rest = do
  let currentResult = eval db current
  let restResult = iterateProgram (resultDb (head currentResult)) rest
  currentResult ++ restResult

iterateProgram :: Database -> [Statement] -> [EvalResult]
iterateProgram db stmts = case stmts of
  []              -> []
  (current:rest)  -> evalStep db current rest

loadProgram :: Database -> [Statement] -> Database
loadProgram db stmts = case stmts of
  [] -> db
  (current:rest) -> loadProgram (resultDb (head (eval db current))) rest

evalProlog :: Database -> Either ParseError Program -> [EvalResult]
evalProlog db ast = case ast of
  Left err    -> [EvalResult db [Error (Left err)]]
  Right prog  -> iterateProgram db prog

--Printer Helpers
printBindings :: [Binding] -> IO ()
printBindings [] = putStrLn "End Binding"
printBindings (current:rest) = do
  putStrLn (Evaluator.name current ++ " = " ++ value current)
  printBindings rest

printSolution :: Solution -> IO ()
printSolution sol = do
  case rule sol of
    Nothing -> putStr "nothing"
    Just x -> printRule x
  putStrLn ""
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