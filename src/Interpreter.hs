{-# LANGUAGE LambdaCase #-}
module Interpreter where

import Lc0
import Util
import Control.Applicative

interpret :: Lc0Expr -> Result Lc0Expr
interpret lambda = Trace "Interpreting..." $ eval (initialState lambda) >>= reverseState

eval :: Lc0State -> Result Lc0State
eval state = isFinal state >>= \case
  True  -> Success state
  False -> trace_state state >>= step >>= inc_steps >>= eval
  where
    inc_steps (Lc0State stack heap steps) = Success (Lc0State stack heap (steps+1))
    trace_state s = Trace (show state)
                  $ reverseState state >>= \expr -> Trace ("Expr: " ++ show expr)
                  $ Success s

initialState :: Lc0Expr -> Lc0State
initialState expr = Lc0State [(addr, mEmpty)] h 0
  where (h, addr) = hAlloc hInitial (NRaw expr)

reverseState :: Lc0State -> Result Lc0Expr
reverseState (Lc0State stack heap _) = hLookup heap x >>= reverse_node
  where
    ((x, _):_) = stack
    reverse_node (NRaw expr)       = Success expr
    reverse_node (NVar addr)       = Var <$> envRevLookup stack addr
    reverse_node (NAp lambda addr) = hLookup heap addr >>= reverse_node >>= \expr -> Success $ Ap (Val lambda) expr

isFinal :: Lc0State -> Result Bool
isFinal (Lc0State ((x, _):_) heap _) = hLookup heap x >>= \case
  (NRaw (Val _)) -> Success True
  _              -> Success False
isFinal (Lc0State [] _ _) = Error $ putStrLn "Empty stack"

step :: Lc0State -> Result Lc0State
step (Lc0State [] _ _ ) = Error $ putStrLn "Empty stack"
step (Lc0State ((x, _):xs) heap steps) = hLookup heap x >>= step_node
  where
    new_state = Lc0State xs heap steps 
    step_node (NRaw expr)      = stepRaw new_state expr
    step_node (NVar addr)      = stepVar new_state addr
    step_node (NAp lambda arg) = stepAp  new_state lambda arg

stepRaw :: Lc0State -> Lc0Expr -> Result Lc0State
--stepRaw (Lc0State stack heap steps) (Val x)        = Success (Lc0State ((loc, mEmpty):stack) heap' steps)
--  where (heap', loc) = hAlloc heap (NAp x _)
stepRaw _                           (Val _)        = Error $ putStrLn "End state reached"
stepRaw (Lc0State stack heap steps) (Var name)     = envLookup stack name >>= \addr -> Success (Lc0State ((addr, mEmpty):stack) heap steps)
stepRaw state                       (Ap (Val a) b) = stepRawAp state (Ap (Val a) b)
stepRaw state                       (Ap a b)       = eval (initialState a) >>= reverseState >>= \a' -> stepRawAp state (Ap a' b) -- TODO add to stack instead of recursion
--stepRaw (Lc0State stack heap steps) (Ap a b)       = Success (Lc0State ((loc, mEmpty):stack) heap' steps)
--  where (heap', loc) = hAlloc heap a
  

stepRawAp :: Lc0State -> Lc0Expr -> Result Lc0State
stepRawAp (Lc0State stack heap steps) (Ap (Val a) b) = Success (Lc0State ((loc_ap, mEmpty):stack) heap'' steps)
  where
    (heap', loc_b) = hAlloc heap (NRaw b)
    (heap'', loc_ap) = hAlloc heap' (NAp a loc_b)
stepRawAp _ _ = Error $ putStrLn "Tried to apply non-lambda expression"

stepVar :: Lc0State -> Addr -> Result Lc0State
stepVar (Lc0State stack heap steps) addr = Success (Lc0State ((addr, mEmpty):stack) heap steps)

stepAp :: Lc0State -> Lc0Lambda -> Addr -> Result Lc0State
stepAp Lc0State {} (Expr _) _                           = Error $ putStrLn "Tried to apply non-lambda expression"
stepAp (Lc0State stack heap steps) (Arg name next) addr = Success (Lc0State ((loc, env):stack) heap' steps)
  where
    (heap', loc) = hAlloc heap (NRaw $ Val next)
    env = (name, addr) : mEmpty 

envLookup :: [(Int, Map Symbol Addr)] -> Symbol -> Result Addr
envLookup ((_, env):xs) s = mLookup env s <|> envLookup xs s
envLookup [] s = Error . putStr $  "'" ++ s ++ "' not found in environment."

envRevLookup :: [(Int, Map Symbol Addr)] -> Addr -> Result Symbol
envRevLookup ((_, (s, a):_):_)   addr | a == addr = Success s
envRevLookup ((x, (_, _):ys):xs) addr             = envRevLookup ((x, ys):xs) addr
envRevLookup ((_, []):xs)        addr             = envRevLookup xs addr
envRevLookup []                  addr             = Error $ putStrLn $ "Address " ++ show addr ++ " not found in environment."
