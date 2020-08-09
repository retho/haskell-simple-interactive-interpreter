module Parser where

import Text.ParserCombinators.Parsec


parseString :: String -> Either ParseError Input
parseString s = parse (inputP <* eof) "(repl)" s

data Input = InputFunctionDeclaration FunctionDeclaration | InputExpression Expression
inputP :: Parser Input
inputP = try (InputFunctionDeclaration <$> functionDeclarationP) <|> (InputExpression <$> expressionP)


{--

function        ::= fn-keyword fn-name { identifier } fn-operator expression
fn-name         ::= identifier
fn-operator     ::= '=>'
fn-keyword      ::= 'fn'

expression      ::= factor | expression operator expression
factor          ::= number | identifier | assignment | '(' expression ')' | function-call
assignment      ::= identifier '=' expression
function-call   ::= fn-name { expression }

operator        ::= '+' | '-' | '*' | '/' | '%'

identifier      ::= letter | '_' { identifier-char }
identifier-char ::= '_' | letter | digit

number          ::= { digit } [ '.' digit { digit } ]

letter          ::= 'a' | 'b' | ... | 'y' | 'z' | 'A' | 'B' | ... | 'Y' | 'Z'
digit           ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

--}


data FunctionDeclaration = FunctionDeclaration Identifier [Identifier] Expression
data Expression = ExpressionFactor Factor | ExpressionOperator Operator Expression Expression deriving (Show)
data Factor = FactorNumber Number | FactorIdentifier Identifier | FactorFunctionCall FunctionCall | FactorAssignment Assignment | FactorParens Expression deriving (Show)
data Assignment = Assignment Identifier Expression deriving (Show)
data FunctionCall = FunctionCall Identifier [Expression] deriving (Show)
data Operator = Operator Char deriving (Show)
data Identifier = Identifier String deriving (Show, Eq, Ord)
data Number = Number Double deriving (Show)

functionDeclarationP :: Parser FunctionDeclaration

expressionP :: Parser Expression
expressionMultiplicativeP :: Parser Expression
expressionAdditiveP :: Parser Expression

factorP :: Parser Factor
factorNumberP :: Parser Factor
factorIdentifierP :: Parser Factor
factorFunctionCallP :: Parser Factor
factorAssignmentP :: Parser Factor
factorParensP :: Parser Factor

assignmentP :: Parser Assignment

operatorMultiplicativeP :: Parser Operator
operatorAdditiveP :: Parser Operator

identifierP :: Parser Identifier
identifierCharP :: Parser Char

functionCallP :: Parser FunctionCall

numberP :: Parser Number

letterP :: Parser Char
digitP :: Parser Char

functionDeclarationP = do
  _ <- string "fn "
  spaces
  fn_name <- identifierP
  fn_args <- many1 $ try (spaces *> identifierP)
  spaces
  _ <- string "=>"
  spaces
  expr <- expressionP
  pure $ FunctionDeclaration fn_name fn_args expr

expressionP = expressionAdditiveP

expressionAdditiveP = do
  let
    expressionAdditiveP' = do
      spaces
      op <- operatorAdditiveP
      spaces
      expr <- expressionMultiplicativeP
      pure (op, expr)
  first <- expressionMultiplicativeP
  rest <- many (try expressionAdditiveP')
  pure $ foldl (\acc (op, expr) -> ExpressionOperator op acc expr) first rest
expressionMultiplicativeP = do
  let
    expressionMultiplicativeP' = do
      spaces
      op <- operatorMultiplicativeP
      spaces
      expr <- ExpressionFactor <$> factorP
      pure (op, expr)
  first <- ExpressionFactor <$> factorP
  rest <- many (try expressionMultiplicativeP')
  pure $ foldl (\acc (op, expr) -> ExpressionOperator op acc expr) first rest

factorP = factorParensP <|> factorNumberP <|> try factorAssignmentP <|> try factorFunctionCallP <|> factorIdentifierP
factorNumberP = FactorNumber <$> numberP
factorIdentifierP = FactorIdentifier <$> identifierP
factorFunctionCallP = FactorFunctionCall <$> functionCallP
factorAssignmentP = FactorAssignment <$> assignmentP
factorParensP = FactorParens <$> (char '(' *> spaces *> expressionP <* spaces <* char ')')

numberP = do
  int <- many1 digitP
  float <- option "" $ do
    first_char <- char '.'
    rest_chars <- many1 digitP
    pure $ first_char:rest_chars
  pure . Number . read $ int <> float

assignmentP = do
  ident <- identifierP
  spaces
  _ <- char '='
  spaces
  expr <- expressionP
  pure $ Assignment ident expr

identifierP = do
  first_char <- letterP <|> char '_'
  rest_chars <- many identifierCharP
  pure . Identifier $ first_char:rest_chars
identifierCharP = char '_' <|> letterP <|> digitP

functionCallP = do
  fn_name <- identifierP
  spaces
  fn_args <- many1 . try $ (ExpressionFactor <$> (try factorNumberP <|> try factorIdentifierP <|> factorParensP)) <* spaces
  pure $ FunctionCall fn_name fn_args

operatorMultiplicativeP = Operator <$> oneOf "*/%"
operatorAdditiveP = Operator <$> oneOf "+-"

letterP = oneOf $ ['a'..'z'] <> ['A'..'Z']
digitP = oneOf ['0'..'9']
