module Main where

import Lib
import System.IO
import Control.Monad

files :: [String]
-- files = ["lc-src/lc1.lc0", "lc-src/test.lc1"]
files = ["lc-src/test.lc0"]

main :: IO ()
main = do
  contents <- mapM get_content files
  printResult True $ execChain contents
  where
    get_content s = openFile s ReadMode >>= hGetContents

execChain :: [String] -> Result Lc0Expr
execChain srcs = do
  translated <- foldM translate l_id srcs
  interpret translated
  where
    l_id = LArg "x" (Var "x")
    translate translator src = Trace "<<<<< FILE >>>>>" 
                             $ interpret (translation translator src)
                           >>= parseProg "intermediate" . decode
