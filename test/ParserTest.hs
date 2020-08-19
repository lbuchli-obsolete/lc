module ParserTest where

import Test.Tasty.HUnit
import Parser
import Util
import Lc0

unit_ParseZeroPlusOne :: IO ()
unit_ParseZeroPlusOne = noTrace (parseProg "test" "(\\m n f x (m f (n f x))) (\\f x (x)) (\\f x (f x))")
                    @?= Success (Ap (Ap (LArg "m" $ LArg "n" $ LArg "f" $ LArg "x" (Ap (Ap (Var "m") (Var "f")) (Ap (Ap (Var "n") (Var "f")) (Var "x"))))
                                     (LArg "f" $ LArg "x" (Var "x")))
                                     (LArg "f" $ LArg "x" (Ap (Var "f") (Var "x"))))
