module InterpreterTest where

import Util
import Lc0
import Interpreter
import Test.Tasty.HUnit

scprop_LeaveIrreducibleExpression :: Int -> Bool
scprop_LeaveIrreducibleExpression x = noTrace (interpret irreducible)
                                   == Success irreducible
  where irreducible = LArg "*" (generatePseudoRandomExpr x)
  
scprop_InterpretApplyIdentity :: Int -> Bool
scprop_InterpretApplyIdentity x = noTrace (interpret (Ap (LArg "@" $ Var "@") irreducible))
                               == Success irreducible
  where irreducible = LArg "*" (generatePseudoRandomExpr x)

unit_RenameAmbiguousVariable :: IO ()
unit_RenameAmbiguousVariable = noTrace (interpret (Ap (LArg "x" $ Var "x") (Var "x")))
                           @?= Success (Var "x")

unit_InterpretZeroPlusOne :: IO ()
unit_InterpretZeroPlusOne = noTrace (interpret (Ap (Ap (LArg "m" $ LArg "n" $ LArg "f" $ LArg "x" (Ap (Ap (Var "m") (Var "f")) (Ap (Ap (Var "n") (Var "f")) (Var "x"))))
                                     (LArg "f" $ LArg "x" (Var "x")))
                                     (LArg "f" $ LArg "x" (Ap (Var "f") (Var "x")))))
                        @?= Success (LArg "f" $ LArg "x" (Ap (Var "f") (Var "x")))
