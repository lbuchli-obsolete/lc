module Main where

import Lib

main :: IO ()
main = printResult False $ parseProg "test" "(\\f a (f a)) (\\x (x)) (\\y (y))" >>= interpret
