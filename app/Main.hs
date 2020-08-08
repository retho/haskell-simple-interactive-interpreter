module Main where

import Control.Monad
import System.IO

import Parser (parseString)


loop :: IO ()
loop = do
  putStr ">>> "
  line <- getLine
  when (length line > 0) $ print $ parseString line
  loop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  loop
