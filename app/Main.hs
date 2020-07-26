module Main where

import Lib
import System.IO

file :: String
file = "lc-src/test.lc0"

main :: IO ()
main = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  printResult True $ parseProg file contents >>= interpret
