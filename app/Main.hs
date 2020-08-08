module Main where

import System.IO

import Interpreter (Interpreter, input, newInterpreter)


loop :: [String] -> Interpreter -> IO ()
loop [] _ = pure ()
loop (line:rest) i
  | length line == 0 = do
      putStr ">>> "
      loop rest i
  | otherwise = do
      case input line i of
        (Left err) -> putStrLn err >> putStr ">>> " >> loop rest i
        (Right (Just r, ii)) -> print r >> putStr ">>> " >> loop rest ii
        (Right (Nothing, ii)) -> putStr ">>> " >> loop rest ii

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  cnts <- getContents
  putStr ">>> "
  loop (lines cnts) newInterpreter
