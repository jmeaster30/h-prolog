module Evaluator where

import Database
import Parser
import Text.Parsec (ParseError)

type Binding = String

newtype Solution = Solution (Either Bool [Binding]) deriving (Eq, Show)

data Result =
    Error ParseError
  | Value [Solution]
  | Quit Bool
  deriving (Eq, Show)

data EvalResult = EvalResult {
  db :: Database,
  result :: Result
} deriving (Eq, Show)

evalProlog :: Database -> Either ParseError Program -> EvalResult
evalProlog db ast = case ast of
  Left err    -> EvalResult db (Error err)
  Right prog  -> EvalResult db (Value [Solution (Left True)])