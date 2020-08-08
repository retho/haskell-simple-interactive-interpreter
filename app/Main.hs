module Main where

import Control.Monad
import System.IO

import Parser (parseString)


loop :: [String] -> IO ()
loop [] = pure ()
loop (line:rest) = do
  when (length line > 0) $ print $ parseString line
  putStr ">>> "
  loop rest

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  cnts <- getContents
  putStr ">>> "
  loop $ lines cnts
