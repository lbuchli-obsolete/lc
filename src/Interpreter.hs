{-# LANGUAGE LambdaCase #-}
module Interpreter where

-- TODO alternative using https://en.wikipedia.org/wiki/Lambda_calculus#Reduction

import Lc0
import Util
import Control.Applicative

interpret :: Lc0Expr -> Result Lc0Expr
interpret lambda = Trace "Interpreting..." $ eval (initialState lambda []) >>= reverseState

eval :: Lc0State -> Result Lc0State
eval state = isFinal state >>= \case
  True  -> Success state
  False -> trace_state state >>= step >>= inc_steps >>= eval
  where
    inc_steps (Lc0State current env heap steps) = Success (Lc0State current env heap (steps+1))
    trace_state s = reverseState state
                >>= \expr -> Trace ("\n----- STEP -----\n--- Expr ---\n" ++ show expr ++ show state) $ Success s

initialState :: Lc0Expr -> [Map Symbol Addr] -> Lc0State
initialState expr init_env = Lc0State addr ([]:init_env) h 0
  where (h, addr) = hAlloc hInitial (NRaw expr)

reverseState :: Lc0State -> Result Lc0Expr
reverseState (Lc0State current env heap _) = hLookup heap current >>= reverse_node
  where
    reverse_node (NRaw expr)       = Success expr
    reverse_node (NVar addr)       = Var <$> envRevLookup env addr
    reverse_node (NAp lambda addr) = hLookup heap addr >>= reverse_node >>= \expr -> Success $ Ap (Val lambda) expr

isFinal :: Lc0State -> Result Bool
isFinal (Lc0State current _ heap _) = hLookup heap current >>= \case
  (NRaw (Val (Arg _ _))) -> Success True
  _                      -> Success False

step :: Lc0State -> Result Lc0State
step state = hLookup heap current >>= step_node
  where
    (Lc0State current _ heap _) = state
    step_node (NRaw expr)      = stepRaw state expr
    step_node (NVar addr)      = stepVar state addr
    step_node (NAp lambda arg) = stepAp  state lambda arg

stepRaw :: Lc0State -> Lc0Expr -> Result Lc0State
--stepRaw (Lc0State stack heap steps) (Val x)        = Success (Lc0State ((loc, mEmpty):stack) heap' steps)
--  where (heap', loc) = hAlloc heap (NAp x _)
stepRaw _                           (Val (Arg _ _)) = Error $ putStrLn "End state reached"
stepRaw state                       (Val (Expr e))  = stepRaw state e
stepRaw (Lc0State _ env heap steps) (Var name)      = envLookup env name >>= \addr -> Success (Lc0State addr env heap steps)
stepRaw state                       (Ap (Val a) b)  = stepRawAp state (Ap (Val a) b)
stepRaw state                       (Ap a b)        = eval (initialState a env) >>= reverseState >>= \a' -> stepRawAp state (Ap a' b)
  where (Lc0State _ env _ _) = state
--stepRaw (Lc0State stack heap steps) (Ap a b)       = Success (Lc0State ((loc, mEmpty):stack) heap' steps)
--  where (heap', loc) = hAlloc heap a
  

stepRawAp :: Lc0State -> Lc0Expr -> Result Lc0State
stepRawAp (Lc0State _ env heap steps) (Ap (Val a) b) = Success (Lc0State loc_ap env heap'' steps)
  where
    (heap', loc_b) = hAlloc heap (NRaw b)
    (heap'', loc_ap) = hAlloc heap' (NAp a loc_b)
stepRawAp _ _ = Error $ putStrLn "Tried to apply non-lambda expression"

stepVar :: Lc0State -> Addr -> Result Lc0State
stepVar (Lc0State _ env heap steps) addr = Success (Lc0State addr env heap steps)

stepAp :: Lc0State -> Lc0Lambda -> Addr -> Result Lc0State
stepAp Lc0State {} (Expr _) _                           = Error $ putStrLn "Tried to apply non-lambda expression"
stepAp (Lc0State _ env heap steps) (Arg name next) addr = Success (Lc0State loc env' heap' steps)
  where
    (heap', loc) = hAlloc heap (NRaw $ Val next)
    (x:xs) = env
    env' = ((name, addr):x):xs

envLookup :: [Map Symbol Addr] -> Symbol -> Result Addr
envLookup env s = foldr (\x -> (<|>) (mLookup x s))
  (Error $ putStrLn $ "Couldn't find '" ++ s ++ "' in environment.")
  env

envRevLookup :: [Map Symbol Addr] -> Addr -> Result Symbol
envRevLookup env a = foldr (\x -> (<|>) (mRevLookup x a))
  (Error $ putStrLn $ "Couldn't find address " ++ show a ++ " in environment.")
  env
