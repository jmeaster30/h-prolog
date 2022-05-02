module Parser where

import Text.Parsec.String ( Parser )
import Text.Parsec.Char
    ( char, alphaNum, letter, lower, noneOf, spaces, string, upper, oneOf )
import Data.Char ()
import Text.Parsec.Combinator
    ( anyToken, eof, many1, manyTill, sepBy, choice, count, optional, chainl, sepBy1, option )
import Text.Parsec ( ParseError, (<|>), many, parse, digit )
import Text.ParserCombinators.Parsec.Token ()
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )

-- AST type definitions!!!!!!!!!!!!!

newtype Variable = Variable {
  varname :: String
} deriving (Eq, Show)

newtype Atom = Atom {
  atomname :: String
} deriving (Eq, Show)

data Term
  = PTAtom     Atom
  | PTVariable Variable
  deriving (Eq, Show)

type ArgList = [Term]
data ProFunctor = ProFunctor {
  funcName :: Term,
  argList :: ArgList
} deriving (Eq, Show)

data ProList
  = PLTerms  [Term]
  | PLString String
  deriving (Eq, Show)

data Compound
  = PCProFunctor ProFunctor
  | PCProList    ProList
  | PCTerm       Term
  deriving (Eq, Show)

data BinaryExpr
  = BEConjunct (Compound, BinaryExpr)
  | BEDisjunct (Compound, BinaryExpr)
  | BEPrimary  Compound
  deriving (Eq, Show)

data Rule = Rule {
  ruleHead :: Compound,
  ruleBody :: BinaryExpr 
} deriving (Eq, Show)

newtype Query = Query {
  queryBody :: Compound
} deriving (Eq, Show)

data Statement
  = PSRule Rule
  | PSQuery Query
  deriving (Eq, Show)

type Program = [Statement]

-- AST builder helpers

createUnary :: String -> String -> Rule
createUnary fname value = Rule (PCProFunctor (ProFunctor (PTAtom (Atom fname)) [PTAtom (Atom value)])) (BEPrimary (PCTerm (PTAtom (Atom "true"))))

-- Parser!!!!!!!!!!!!!

parseProlog :: String -> Either ParseError Program
parseProlog = regularParse prologProgram

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof

prologVariable :: Parser Variable
prologVariable = do
  varUpper <- upper <|> char '_';
  varRest <- many (alphaNum <|> char '_');
  return (Variable (varUpper : varRest))

prologEmptyAtom :: Parser Atom
prologEmptyAtom = do
  result <- string "[]"
  return (Atom result)

prologQuoteAtom :: Parser Atom
prologQuoteAtom = do
  char '\''
  result <- many (noneOf "\'")
  char '\''
  return (Atom result)

prologBareAtom :: Parser Atom
prologBareAtom = do
  atomStart <- lower
  atomRest <- many (alphaNum <|> char '_')
  return (Atom (atomStart : atomRest))

prologNumber :: Parser Atom
prologNumber = (do {
  sign <- char '+'; number <- many digit; return (Atom (sign:number))
}) <|> (do {
  sign <- char '-'; number <- many digit; return (Atom (sign:number))
}) <|> (do {
  number <- many1 digit; return (Atom number)
})

prologAtom :: Parser Atom
prologAtom = prologEmptyAtom <|> prologQuoteAtom <|> prologBareAtom <|> prologNumber

prologTerm :: Parser Term
prologTerm = PTAtom <$> prologAtom <|> PTVariable <$> prologVariable

prologArgSeperator :: Parser ()
prologArgSeperator  = do
  char ','
  spaces

prologFunctorFollow :: Parser [Term]
prologFunctorFollow = do
  spaces
  char '('
  spaces
  arglist <- prologTerm `sepBy` prologArgSeperator
  spaces
  char ')';
  return arglist

prologFunctor :: Parser ProFunctor
prologFunctor = do
  functor <- prologTerm
  arglist <- option [] prologFunctorFollow
  return (ProFunctor functor arglist)

prologString :: Parser String
prologString = do
  char '"'
  result <- manyTill anyToken (char '"')
  char '"'
  return result

prologNestedList :: String -> String -> String -> Parser [Term]
prologNestedList left sep right = choice [
  count 1 prologTerm,
  prologNestedListRecurse left sep right
  ]

prologNestedListRecurse :: String -> String -> String -> Parser [Term]
prologNestedListRecurse left sep right = do
  string left
  spaces
  head <- prologTerm
  spaces
  string sep
  spaces
  tail <- prologNestedList left sep right
  spaces
  string right
  return (head:tail)

prologFlatList :: Char -> Char -> Char -> Parser [Term]
prologFlatList left sep right = do
  char left
  spaces
  result <- prologTerm `sepBy1` prologArgSeperator
  char right
  return result

prologList :: Parser [Term]
prologList = prologFlatList '[' ',' ']' <|> prologNestedList ".(" "," ")"

prologProList :: Parser ProList
prologProList = PLTerms <$> prologList <|> PLString <$> prologString

prologCompound :: Parser Compound
prologCompound = choice [
  PCProFunctor <$> prologFunctor,
  PCProList <$> prologProList 
  ]

prologBinaryFollow :: Compound -> Parser BinaryExpr
prologBinaryFollow head = (do {
  string ",";
  spaces;
  next <- prologCompound;
  follow <- prologBinaryFollow next;
  return (BEConjunct (head, follow))
}) <|> (do {
  string ";";
  spaces;
  next <- prologCompound;
  follow <- prologBinaryFollow next;
  return (BEDisjunct (head, follow))
}) <|> (do {
  return (BEPrimary head)
})

prologBinaryExpr :: Parser BinaryExpr
prologBinaryExpr = do
  first <- prologCompound
  spaces
  prologBinaryFollow first

prologRule :: Parser Rule
prologRule = do
  head <- prologCompound
  spaces
  Rule head <$> prologRuleFollow

prologRuleFollow :: Parser BinaryExpr
prologRuleFollow = (do {
  string ":-";
  spaces;
  body <- prologBinaryExpr;
  spaces;
  char '.';
  return body
}) <|> (do {
  char '.';
  return (BEPrimary (PCTerm (PTAtom (Atom "true"))))
})

prologQuery :: Parser Query
prologQuery = do
  string "?"
  spaces
  result <- Query <$> prologCompound
  char '.'
  return result

prologStatement :: Parser Statement
prologStatement = do
  spaces
  (PSRule <$> prologRule) <|> (PSQuery <$> prologQuery)

prologProgram :: Parser Program
prologProgram = do
  sepBy prologStatement (many (char '\n'))

-- AST printers

printVariable :: Variable -> IO ()
printVariable = putStr . varname

printAtom :: Atom -> IO ()
printAtom = putStr . atomname

printTerm :: Term -> IO ()
printTerm (PTAtom atom)     = printAtom atom
printTerm (PTVariable var)  = printVariable var

printTermListStep :: Term -> [Term] -> IO ()
printTermListStep curr rest = do
  printTerm curr
  case rest of
    [] -> putStr ""
    (next:nrest) -> (do {
      putStr ", ";
      printTermListStep next nrest 
    })

printArgList :: ArgList -> IO ()
printArgList (top:rest) = printTermListStep top rest
printArgList [] = putStr ""

printFunctor :: ProFunctor -> IO ()
printFunctor func = do
  printTerm (funcName func)
  putStr "("
  printArgList (argList func)
  putStr ")"

printProList :: ProList -> IO ()
printProList (PLString str) = putStr ("\"" ++ str ++ "\"")
printProList (PLTerms (top:rest)) = do
  putStr "["
  printTermListStep top rest
  putStr "]"
printProList (PLTerms []) = putStr "[]"

printCompound :: Compound -> IO ()
printCompound (PCProFunctor func) = printFunctor func
printCompound (PCProList plist) = printProList plist
printCompound (PCTerm term) = printTerm term

printPBinaryExpr :: BinaryExpr -> IO ()
printPBinaryExpr (BEConjunct (head, next)) = do
  printCompound head
  putStr ", "
  printPBinaryExpr next
printPBinaryExpr (BEDisjunct (head, next)) = do
  printCompound head
  putStr "; "
  printPBinaryExpr next
printPBinaryExpr (BEPrimary comp) = printCompound comp

printRule :: Rule -> IO ()
printRule rule = do
  printCompound (ruleHead rule)
  putStr " :- "
  printPBinaryExpr (ruleBody rule)

printQuery :: Query -> IO ()
printQuery query = do
  putStr "?"
  printCompound (queryBody query)

printStatement :: Statement -> IO ()
printStatement (PSRule rule) = do 
  printRule rule
  putStrLn "."
printStatement (PSQuery query) = do
  printQuery query
  putStrLn "."

debugPrint :: Either ParseError Program -> IO ()
debugPrint test = case test of
  Left error -> print error
  Right prog -> putStrLn "good :)"
