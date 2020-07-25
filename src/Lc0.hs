module Lc0 where

import Util

type Symbol = String

data Lc0Lambda
  = Arg Symbol Lc0Lambda
  | Expr Lc0Expr
  deriving (Eq, Show)
  
data Lc0Expr
  = Val Lc0Lambda
  | Var Symbol
  | Ap Lc0Expr Lc0Expr
  deriving (Eq, Show)


data Lc0State = Lc0State {
  sCurrent :: Addr,
  sEnv :: Map Symbol Addr,
  sHeap  :: Heap Lc0Node,
  sSteps :: Int
}

instance Show Lc0State where
  show (Lc0State current env heap steps) = s_current ++ s_env ++ show heap ++ s_steps 
    where
      s_current = "--- Current Address ---\n"
        ++ show current
      s_env = "--- Environment (" ++ show (length env) ++ ") ---"
        ++ foldl (\prev (s, a) -> prev ++ "\n" ++ s ++ "\t=> " ++ show a) "" env
      s_steps = "--- Steps ---\n" ++ show steps

data Lc0Node
  = NRaw Lc0Expr
  | NVar Addr
  | NAp  Lc0Lambda Addr
  deriving (Eq, Show)
