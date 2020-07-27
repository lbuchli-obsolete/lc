module Lc0 where

import Util

type Symbol = String

data Lc0Expr
  = LArg Symbol Lc0Expr
  | Var Symbol
  | Ap Lc0Expr Lc0Expr
  deriving Eq

instance Show Lc0Expr where
  show (LArg n e)  = "(\\" ++ n ++ " " ++ show e ++ ")"
  show (Var s)  = s
  show (Ap a b) = "(" ++ show a  ++ " " ++ show b ++ ")"

data Lc0State = Lc0State {
  sCurrent :: Lc0Expr,
  sEnv :: Map Symbol Lc0Expr,
  sSteps :: Int
}

instance Show Lc0State where
  show (Lc0State current env steps) = "<<< STATE >>>\n" ++ s_current ++ s_env ++ s_steps 
    where
      s_current = "\n--- Current Expression ---\n"
        ++ show current ++ "\n"
      s_env = "--- Environment (" ++ show (length env) ++ ") ---"
        ++ foldl (\prev (s, a) -> prev ++ "\n" ++ s ++ "\t=> " ++ show a) "" env ++ "\n"
      s_steps = "--- Steps ---\n" ++ show steps
