{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lc0 where

import Util
import System.Random
import Data.Bifunctor

type Symbol = String

data Lc0Expr
  = LArg Symbol Lc0Expr
  | Var Symbol
  | Ap Lc0Expr Lc0Expr
  | ApVar Symbol Int Lc0Expr -- this is for pure performance reasons and could be left out

instance Eq Lc0Expr where
  (==) a b = eq a b []
    where
      -- alpha equivalence
      eq (LArg sa ea)     (LArg sb eb)     env = eq ea eb ((sa, sb):env)
      eq (Var sa)         (Var sb)         env = linked (link_if_not_in env sa sb) sa sb
      eq (Ap aa ba)       (Ap ab bb)       env = eq aa ab env && eq ba bb env
      eq (ApVar sa ta ea) (ApVar sb tb eb) env = linked env' sa sb && ta == tb && eq ea eb env'
        where env' = link_if_not_in env sa sb
      eq _                _                _   = False
      linked ((a', b'):_) a'' b'' | a' == a'' && b' == b'' = True
      linked (_:xs)       a'' b''                          = linked xs a'' b''
      linked []          _  _                              = False
      link_if_not_in env a'' b'' | contains_link env a'' b'' = env
      link_if_not_in env a'' b''                             = (a'', b''):env
      contains_link env a'' b'' = any (\(a', b') -> a'' == a' || b'' == b') env
{-
instance Monad m => Serial m Lc0Expr {- where
  series = cons2 LArg
        \/ cons1 Var
        \/ cons2 Ap
        \/ cons3 ApVar
-}-}
  

-- TODO using this breaks Smallchecks property to show examples, but I can't get the generic generator for Lc0Expr working.
generatePseudoRandomExpr :: Int -> Lc0Expr
generatePseudoRandomExpr x = fst $ generate $ mkStdGen x

  
renameVar :: (Symbol -> Int -> Symbol) -> Lc0Expr -> Lc0Expr
renameVar = rename_var 0
  where
    rename_var x f (LArg n expr)  = LArg (f n (x+1)) (rename_var (x+1) f expr)
    rename_var x f (Var n)        = Var (f n x)
    rename_var x f (Ap a b)       = Ap (rename_var x f a) (rename_var x f b)
    rename_var x f (ApVar n t b)  = ApVar (f n x) t (rename_var x f b)
    
instance Show Lc0Expr where
  show (LArg n e)  = "(\\" ++ n ++ " " ++ showLArg e ++ ")"
  show (ApVar n t b) = "(" ++ n ++ "*" ++ show t ++ " " ++ show b ++ ")"
  show (Var s)  = s
  show (Ap a b) = "(" ++ show a  ++ " " ++ show b ++ ")"

showLArg :: Lc0Expr -> String
showLArg (LArg n e) = n ++ " " ++ showLArg e
showLArg x = show x 

instance Arbitrary Lc0Expr where
  generate gen = generateExpr gen ["x"]

generateExpr :: StdGen -> [String] -> (Lc0Expr, StdGen)
generateExpr gen vars = case n `mod` 7 of
                          0 -> generate_larg
                          1 -> generate_ap
                          2 -> generate_apvar
                          _ -> generate_var
  where
    (n, g) = next gen
    generate_larg = let name = take 2 $ randomRs ('a', 'z') g in
      first (LArg name) (generateExpr g (name:vars)) 
    generate_var = (,)
      (Var (vars !! (n `mod` length vars)))
      g
    generate_ap = (,)
      (let (g1, g2) = split g in Ap (fst $ generateExpr g1 vars) (fst $ generateExpr g2 vars))
      g
    generate_apvar = let (n', g') = next g in
      first (ApVar (vars !! (n `mod` length vars)) (n' `mod` 50)) (generateExpr g' vars) 

data Lc0State = Lc0State {
  sCurrent :: Lc0Expr,
  sEnv :: Map Symbol Lc0Expr,
  sSteps :: Int
}

{-
instance Monad m => Serial m Lc0State where
  series = cons3 Lc0State
-}

instance Show Lc0State where
  show (Lc0State current env steps) = "<<< STATE >>>" ++ s_current ++ s_env ++ s_steps 
    where
      s_current = "\n--- Current Expression ---\n"
        ++ show current ++ "\n"
      s_env = "--- Environment (" ++ show (length env) ++ ") ---"
        ++ foldl (\prev (s, a) -> prev ++ "\n" ++ s ++ "\t=> " ++ show a) "" env ++ "\n"
      s_steps = "--- Steps ---\n" ++ show steps
