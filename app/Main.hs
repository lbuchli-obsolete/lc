module Main where

import Lib

main :: IO ()
main = printResult True $ parseProg "test" "(\\f a (f a)) (\\x (x)) (\\y (y))" >>= interpret
