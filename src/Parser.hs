module Parser where

import Text.ParserCombinators.Parsec


parseString :: String -> Either ParseError Expression
parseString s = parse (expressionP <* eof) "(repl)" s


{--

expression      ::= factor | expression operator expression
factor          ::= number | identifier | assignment | '(' expression ')'
assignment      ::= identifier '=' expression

operator        ::= '+' | '-' | '*' | '/' | '%'

identifier      ::= letter | '_' { identifier-char }
identifier-char ::= '_' | letter | digit

number          ::= { digit } [ '.' digit { digit } ]

letter          ::= 'a' | 'b' | ... | 'y' | 'z' | 'A' | 'B' | ... | 'Y' | 'Z'
digit           ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

--}


data Expression = ExpressionFactor Factor | ExpressionOperator Operator Expression Expression deriving (Show)
data Factor = FactorNumber Number | FactorIdentifier Identifier | FactorAssignment Assignment | FactorParens Expression deriving (Show)
data Assignment = Assignment Identifier Expression deriving (Show)
data Operator = Operator Char deriving (Show)
data Identifier = Identifier String deriving (Show, Eq, Ord)
data Number = Number Double deriving (Show)


expressionP :: Parser Expression
expressionMultiplicativeP :: Parser Expression
expressionAdditiveP :: Parser Expression

factorP :: Parser Factor
factorNumberP :: Parser Factor
factorIdentifierP :: Parser Factor
factorAssignmentP :: Parser Factor
factorParensP :: Parser Factor

assignmentP :: Parser Assignment

operatorMultiplicativeP :: Parser Operator
operatorAdditiveP :: Parser Operator

identifierP :: Parser Identifier
identifierCharP :: Parser Char

numberP :: Parser Number

letterP :: Parser Char
digitP :: Parser Char


expressionP = try expressionAdditiveP <|> try expressionMultiplicativeP <|> (ExpressionFactor <$> factorP)
expressionMultiplicativeP = do
  expr1 <- ExpressionFactor <$> factorP
  spaces
  op <- operatorMultiplicativeP
  spaces
  expr2 <- try expressionMultiplicativeP <|> (ExpressionFactor <$> factorP)
  pure $ ExpressionOperator op expr1 expr2
expressionAdditiveP = do
  expr1 <- try expressionMultiplicativeP <|> (ExpressionFactor <$> factorP)
  spaces
  op <- operatorAdditiveP
  spaces
  expr2 <- try expressionAdditiveP <|> try expressionMultiplicativeP <|> (ExpressionFactor <$> factorP)
  pure $ ExpressionOperator op expr1 expr2

factorP = factorParensP <|> factorNumberP <|> try factorAssignmentP <|> factorIdentifierP
factorNumberP = FactorNumber <$> numberP
factorIdentifierP = FactorIdentifier <$> identifierP
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

operatorMultiplicativeP = Operator <$> oneOf "*/%"
operatorAdditiveP = Operator <$> oneOf "+-"

letterP = oneOf $ ['a'..'z'] <> ['A'..'Z']
digitP = oneOf ['0'..'9']

