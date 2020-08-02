module Interpreter where

data Interpreter
type Result = Maybe Double

newInterpreter :: Interpreter
newInterpreter = undefined


input :: String -> Interpreter -> Either String (Result, Interpreter)
input _ _ = Left "Not implemented"
