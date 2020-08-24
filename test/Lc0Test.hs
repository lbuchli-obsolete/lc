module Lc0Test where

import Lc0
import Test.Tasty.HUnit

scprop_AlphaEquivalence :: Int -> Bool
scprop_AlphaEquivalence x = expr == expr'
  where
    expr  = generatePseudoRandomExpr x
    expr' = renameVar shiftNames expr
    shiftNames s t = s ++ "-" ++ show t 

unit_RenameVar :: IO ()
unit_RenameVar = renameVar (\x t -> x ++ replicate t '*') (Ap (Var "x") (ApVar "x" 32 (LArg "x" $ Var "x")))
             @?= Ap (Var "x") (ApVar "x" 32 (LArg "x*" $ Var "x*"))
