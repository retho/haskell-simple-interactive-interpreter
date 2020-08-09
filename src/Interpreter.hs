module Interpreter where

import Control.Monad
import qualified Data.List as L
import Data.Map hiding (map)

import Parser

type Scope = Map Identifier Rep
data Interpreter = Interpreter {globalScope :: Scope}
type Result = Maybe Double


-- runtime representation
data Rep = RepNum Double | RepFn FunctionDeclaration

instance Show Interpreter where show = const "[Interpreter]"

newInterpreter :: Interpreter
newInterpreter = Interpreter {globalScope = empty}


input :: String -> Interpreter -> Either String (Result, Interpreter)
input source i0 = case parseString source of
  Left err -> Left (show err)
  Right inp -> case input' inp i0 of
    Left err -> Left err
    Right (RepNum val, i1) -> Right (Just val, i1)
    Right (_, i1) -> Right (Nothing, i1)


input' :: Input -> Interpreter -> Either String (Rep, Interpreter)
input' (InputExpression expr) i = eval empty expr i
input' (InputFunctionDeclaration fn@(FunctionDeclaration fn_name fn_args expr)) i =
  case L.find (`notElem` fn_args <> keys (globalScope i)) $ findIdents expr of
    Just (Identifier ident) -> Left $ "ERROR: Invalid identifier " <> "'" <> ident <> "'" <> " in function body"
    Nothing -> case globalScope i !? fn_name of
      Just (RepNum _) -> Left $ "ERROR: identifier " <> "'" <> fn_name_str <> "'" <> " is not a function" where Identifier fn_name_str = fn_name
      _ -> pure (RepFn fn, i {globalScope = insert fn_name (RepFn fn) $ globalScope i})


findIdents :: Expression -> [Identifier]
findIdents (ExpressionFactor (FactorNumber (Number _))) = []
findIdents (ExpressionFactor (FactorIdentifier ident)) = [ident]
findIdents (ExpressionFactor (FactorFunctionCall (FunctionCall fn_name args_exprs))) =
  fn_name : join (map findIdents args_exprs)
findIdents (ExpressionFactor (FactorAssignment (Assignment _ expr))) = findIdents expr
findIdents (ExpressionFactor (FactorParens expr)) = findIdents expr
findIdents (ExpressionOperator _ expr1 expr2) = findIdents expr1 <> findIdents expr2


eval :: Scope -> Expression -> Interpreter -> Either String (Rep, Interpreter)
eval _ (ExpressionFactor (FactorNumber (Number n))) i = pure (RepNum n, i)
eval scope (ExpressionFactor (FactorIdentifier ident)) i =
  case scope `union` globalScope i !? ident of
    Nothing -> Left $ "ERROR: Unknown identifier " <> "'" <> ident_name <> "'" where Identifier ident_name = ident
    Just val -> Right (val, i)
eval scope (ExpressionFactor (FactorFunctionCall (FunctionCall fn_name args_exprs))) i0 =
  case scope `union` globalScope i0 !? fn_name of
    Nothing -> Left $ "ERROR: Unknown identifier " <> "'" <> ident_name <> "'" where Identifier ident_name = fn_name
    Just val@(RepNum _) -> Right (val, i0)
    Just (RepFn fn) -> do
      (args, i1) <- loop args_exprs i0
      evalFnCall scope fn args i1
      where
          loop :: [] Expression -> Interpreter -> Either String ([Rep], Interpreter)
          loop [] i = Right ([], i)
          loop (e:xprs) ii0 = do
            (val, ii1) <- eval scope e ii0
            (rest_vals, ii2) <- loop xprs ii1
            pure (val:rest_vals, ii2)
eval scope (ExpressionFactor (FactorAssignment (Assignment ident expr))) i0 = do
  (val, i1) <- eval scope expr i0
  case globalScope i1 !? ident of
     Just (RepFn _) -> Left $ "ERROR: identifier " <> "'" <> ident_str <> "'" <> " is a function" where Identifier ident_str = ident
     _ -> pure (val, i1 {globalScope = insert ident val $ globalScope i1})
eval scope (ExpressionFactor (FactorParens expr)) i = eval scope expr i
eval scope (ExpressionOperator op expr1 expr2) i0 = do
  (x, i1) <- eval scope expr1 i0
  (y, i2) <- eval scope expr2 i1
  r <- calc op x y
  pure (r, i2)


evalFnCall :: Scope -> FunctionDeclaration -> [] Rep -> Interpreter -> Either String (Rep, Interpreter)
evalFnCall scope0 (FunctionDeclaration fn_name fn_args expr) args i
  | length fn_args /= length args = let Identifier fn_name_str = fn_name in Left
    $ "ERROR: function "
    <> "'" <> fn_name_str <> "'"
    <> " expect "
    <> show (length fn_args)
    <> " arguments, but "
    <> show (length args)
    <> " found"
  | otherwise = eval scope1 expr i
      where scope1 = fromList (zip fn_args args) `union` scope0 `union` globalScope i


calc :: Operator -> Rep -> Rep -> Either String Rep
calc (Operator '+') (RepNum x) (RepNum y) = pure . RepNum $ x + y
calc (Operator '+') _ _ = error "Can perform calculations only for numbers"
calc (Operator '-') (RepNum x) (RepNum y) = pure . RepNum $ x - y
calc (Operator '-') _ _ = error "Can perform calculations only for numbers"
calc (Operator '*') (RepNum x) (RepNum y) = pure . RepNum $ x * y
calc (Operator '*') _ _ = error "Can perform calculations only for numbers"
calc (Operator '/') (RepNum x) (RepNum y) = pure . RepNum $ x / y
calc (Operator '/') _ _ = error "Can perform calculations only for numbers"
calc (Operator '%') (RepNum x) (RepNum y) = pure . RepNum $ x - y * fromIntegral n
  where n :: Int = truncate $ x / y
calc (Operator '%') _ _ = error "Can perform calculations only for numbers"
calc (Operator c) _ _ = error $ "Unknown operator " <> show c
