module Main where

import System.IO

import Interpreter (Interpreter, input, newInterpreter)


loop :: [String] -> Interpreter -> IO ()
loop [] _ = pure ()
loop (line:rest) i0
  | length line == 0 = do
      putStr ">>> "
      loop rest i0
  | otherwise = do
      case input line i0 of
        (Left err) -> putStrLn err >> putStr ">>> " >> loop rest i0
        (Right (Just r, i1)) -> print r >> putStr ">>> " >> loop rest i1
        (Right (Nothing, i1)) -> putStr ">>> " >> loop rest i1

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  cnts <- getContents
  putStr ">>> "
  loop (lines cnts) newInterpreter
