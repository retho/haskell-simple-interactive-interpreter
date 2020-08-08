module Interpreter where

import Data.Map

import Parser


data Interpreter = Interpreter {assignments :: Map Identifier Rep}
type Result = Maybe Double


-- runtime representation
data Rep = RepNumber Double

instance Show Interpreter where show = const "[Interpreter]"

newInterpreter :: Interpreter
newInterpreter = Interpreter {assignments = empty}


input :: String -> Interpreter -> Either String (Result, Interpreter)
input source i = case parseString source of
  Left err -> Left (show err)
  Right expr -> case eval expr i of
    Left err -> Left err
    Right (RepNumber val, ii) -> Right (Just val, ii)


eval :: Expression -> Interpreter -> Either String (Rep, Interpreter)
eval (ExpressionFactor (FactorNumber (Number n))) i = pure (RepNumber n, i)
eval (ExpressionFactor (FactorIdentifier ident@(Identifier ident_name))) i =
  case assignments i !? ident of
    Nothing -> Left $ "ERROR: Unknown identifier " <> "'" <> ident_name <> "'"
    Just val -> Right (val, i)
eval (ExpressionFactor (FactorAssignment (Assignment ident expr))) i0 = do
  (val, i1) <- eval expr i0
  let i2 = Interpreter {assignments = insert ident val $ assignments i1}
  pure (val, i2)
eval (ExpressionFactor (FactorParens expr)) i0 = eval expr i0
eval (ExpressionOperator op expr1 expr2) i0 = do
  (x, i1) <- eval expr1 i0
  (y, i2) <- eval expr2 i1
  r <- calc op x y
  pure (r, i2)


calc :: Operator -> Rep -> Rep -> Either String Rep
calc (Operator '+') (RepNumber x) (RepNumber y) = pure . RepNumber $ x + y
calc (Operator '-') (RepNumber x) (RepNumber y) = pure . RepNumber $ x - y
calc (Operator '*') (RepNumber x) (RepNumber y) = pure . RepNumber $ x * y
calc (Operator '/') (RepNumber x) (RepNumber y) = pure . RepNumber $ x / y
calc (Operator '%') (RepNumber x) (RepNumber y) = pure . RepNumber $ x - y * fromIntegral n
  where n :: Int = truncate $ x / y
calc (Operator c) _ _ = error $ "Unknown operator " <> show c
