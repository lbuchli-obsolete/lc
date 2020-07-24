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
  sStack :: [(Addr, Map Symbol Addr)],
  sHeap  :: Heap Lc0Node,
  sSteps :: Int
}

instance Show Lc0State where
  show (Lc0State stack heap steps) = s_stack ++ show heap ++ s_steps 
    where
      s_stack = "--- Stack (" ++ show (length stack) ++ ") ---\n"
        ++ foldl (\prev a -> prev ++ show a ++ " ") "" stack ++ "\n"
      s_steps = "--- Steps ---\n" ++ show steps

data Lc0Node
  = NRaw Lc0Expr
  | NVar Addr
  | NAp  Lc0Lambda Addr
  deriving (Eq, Show)
