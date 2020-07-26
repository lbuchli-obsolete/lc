module Lc0 where

import Util

type Symbol = String

data Lc0Lambda
  = Arg Symbol Lc0Lambda
  | Expr Lc0Expr
  deriving Eq

instance Show Lc0Lambda where
  show (Arg s l) = s ++ " " ++ show l
  show (Expr e)  = show e
  
data Lc0Expr
  = Val Lc0Lambda
  | Var Symbol
  | Ap Lc0Expr Lc0Expr
  deriving Eq

instance Show Lc0Expr where
  show (Val l)  = "(\\" ++ show l ++ ")"
  show (Var s)  = s
  show (Ap a b) = "(" ++ show a  ++ " " ++ show b ++ ")"

data Lc0State = Lc0State {
  sCurrent :: Addr,
  sEnv :: [Map Symbol Addr],
  sHeap  :: Heap Lc0Node,
  sSteps :: Int
}

instance Show Lc0State where
  show (Lc0State current env heap steps) = s_current ++ s_env ++ show heap ++ s_steps 
    where
      s_current = "\n--- Current Address ---\n"
        ++ show current ++ "\n"
      s_env = "--- Environment (" ++ show (length env) ++ ") ---"
        ++ foldl (\prev n -> prev ++ s_part_env n ++ "---") "" env ++ "\n"
      s_steps = "--- Steps ---\n" ++ show steps
      s_part_env m = foldl (\prev (s, a) -> prev ++ "\n" ++ s ++ "\t=> " ++ show a) "" m ++ "\n"

data Lc0Node
  = NRaw Lc0Expr
  | NVar Addr
  | NAp  Lc0Lambda Addr
  deriving (Eq, Show)
