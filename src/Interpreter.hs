{-# LANGUAGE LambdaCase #-}
module Interpreter where

-- TODO alternative using https://en.wikipedia.org/wiki/Lambda_calculus#Reduction

import Lc0
import Util
import Control.Applicative

interpret :: Lc0Expr -> Result Lc0Expr
interpret expr = Trace "Interpreting..." $ eval (initialState (renameVar arg' expr) []) >>= reverseState
  where arg' x t = x ++ replicate t '\''

eval :: Lc0State -> Result Lc0State
eval state = isFinal state >>= \case
  True  -> Success state
  False -> trace_state state >>= step >>= inc_steps >>= eval
  where
    inc_steps (Lc0State _       _   65536) = Error $ putStrLn "Too many steps (Max is 2^16)"
    inc_steps (Lc0State current env steps) = Success (Lc0State current env (steps+1))
    trace_state s = Trace (show s) $ Success s

initialState :: Lc0Expr -> Map Symbol Lc0Expr -> Lc0State
initialState expr init_env = Lc0State expr init_env 0

reverseState :: Lc0State -> Result Lc0Expr
{-
reverseState (Lc0State (LArg name expr) env steps) | any (\(n, _) -> n == name) env = reverseState (Lc0State (renameVar name (name ++ "'") (LArg name expr)) env steps)
reverseState (Lc0State (LArg name expr) env steps) = LArg name <$> reverseState (Lc0State expr env steps)
reverseState (Lc0State (Var name) env steps)       = (mLookup env name >>= \expr -> reverseState (Lc0State expr env steps)) <|> pure (Var name)
reverseState (Lc0State (Ap a b) env steps)         = Ap <$> reverseState (Lc0State a env steps) <*> reverseState (Lc0State b env steps)
reverseState (Lc0State (ApVar _ 0 b) env steps)    = reverseState (Lc0State b env steps)
reverseState (Lc0State (ApVar n t b) env steps)    = reverseState (Lc0State (Ap (Var n) (ApVar n (t-1) b)) env steps)
-}
reverseState (Lc0State (LArg name expr) env steps) = LArg name <$> reverseState (Lc0State expr (mDeleteAll env name) steps)
reverseState (Lc0State (Var name) env steps)       = (mLookup env name >>= \expr -> reverseState (Lc0State expr env steps)) <|> pure (Var name)
reverseState (Lc0State (Ap a b) env steps)         = Ap <$> reverseState (Lc0State a env steps) <*> reverseState (Lc0State b env steps)
reverseState (Lc0State (ApVar n t b) env steps)    = ApVar n t <$> reverseState (Lc0State b env steps)
   
isFinal :: Lc0State -> Result Bool
isFinal (Lc0State current env _) = pure $ case current of
  (LArg _ _) -> case lookup "" env of
    Just _   -> False
    Nothing  -> True
  _          -> False

step :: Lc0State -> Result Lc0State
step state = case current of
  (LArg name expr) -> stepLArg state name expr
  (Var name)       -> stepVar state name
  (Ap a b)         -> stepAp state a b
  (ApVar _ 0 b)    -> step (Lc0State b env steps)
  (ApVar n t b)    -> stepAp state (Var n) (ApVar n (t-1) b)
  where
    state' = (\(Lc0State c e s) -> Lc0State (compress c) e s) state
    (Lc0State current env steps) = state'
    var_ap_depth s (Ap (Var a) b) | s == a = 1 + var_ap_depth s b
    var_ap_depth _ _                       = 0
    compress (Ap (Var a) b) = case var_ap_depth a b of
      0 -> Ap (Var a) b
      x -> ApVar a (x+1) b
    compress other          = other

stepLArg :: Lc0State -> Symbol -> Lc0Expr -> Result Lc0State
stepLArg (Lc0State _ env steps) arg expr = (\env' -> Lc0State expr env' steps) <$> claimEnvVar env arg

stepVar :: Lc0State -> Symbol -> Result Lc0State
stepVar (Lc0State _ env steps) name = (\expr -> Lc0State expr env steps) <$> mLookup env name 

stepAp :: Lc0State -> Lc0Expr -> Lc0Expr -> Result Lc0State
stepAp (Lc0State _ env steps) a b = pure $ Lc0State a (("", b):env) steps

claimEnvVar :: Map Symbol Lc0Expr -> Symbol -> Result (Map Symbol Lc0Expr)
claimEnvVar [] _           = Error $ putStrLn "No free variable to claim"
claimEnvVar ((s, e):xs) s' = claimThis <|> claimOther
  where
    claimThis = case s of
      "" -> Success ((s', e):xs)
      _  -> Error $ putStrLn $ "'" ++ s ++ "' is not a free variable"
    claimOther = (:) (s, e) <$> claimEnvVar xs s'

renameStateVar :: (Symbol -> Int -> Symbol) -> Lc0State -> Lc0State
renameStateVar f (Lc0State expr env steps) = Lc0State expr' env' steps
  where
    expr' = renameVar f expr
    env'  = map (\(k, v) -> (f k 0, v)) env
