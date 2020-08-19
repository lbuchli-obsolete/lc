module Converter where

import Data.Char
import Lc0

{-
Set up a translation using a source string and a translator expression
-}
translation :: Lc0Expr -> String -> Lc0Expr
translation translator src = Ap translator (encode src)

{-
Encode a string using church encoding for individual characters
that are then linked in a list as pairs
-}
encode :: String -> Lc0Expr
encode str = list_encode $ map (church_encode . ord) str
  where
    church_encode x = LArg "f" $ LArg "x" $ ApVar "f" x (Var "x")
--    ap_wrap _ 0 b = b
--    ap_wrap a x b = Ap a $ ap_wrap a (x-1) b
    list_encode (n:ns) = LArg "f" $ Ap (Ap (Var "f") n) (list_encode ns)
    list_encode []     = Ap (Var "x") (LArg "a" (LArg "b" (Var "a"))) -- NIL

{-
Decode an encoded string
-}
decode :: Lc0Expr -> String
decode expr = map (chr . church_decode) $ list_decode expr
  where
    church_decode (LArg _ (LArg _ e)) = ap_count e
    church_decode _                   = -1
    ap_count (Ap a b)      = 1 + ap_count a + ap_count b
    ap_count (ApVar _ t b) = t + ap_count b
    ap_count _             = 0
    list_decode (LArg f (Ap (Ap (Var f') n) ns)) | f == f' = n : list_decode ns
    list_decode _                                          = []
