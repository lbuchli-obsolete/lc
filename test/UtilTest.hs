module UtilTest where

import Util

scprop_ResultFunctorIdentity :: Result Int -> Bool
scprop_ResultFunctorIdentity res = fmap id res == res

scprop_ResultApplicativeHomomorphism :: Int -> Int -> Bool
scprop_ResultApplicativeHomomorphism f x = (pure (+f) <*> pure x) == (pure (f + x) :: Result Int)

scprop_ResultApplicativeInterchange :: Result Int -> Int -> Bool
scprop_ResultApplicativeInterchange u y = (fmap (+) u <*> pure y) == (pure ($ y) <*> fmap (+) u)

scprop_ResultMonadLeftIdentity :: Int -> Result Int -> Bool
scprop_ResultMonadLeftIdentity a k = (return a >>= \x -> fmap (+x) k) == (\x -> fmap (+x) k) a

scprop_ResultMonadRightIdentity :: Result Int -> Bool
scprop_ResultMonadRightIdentity m = (m >>= return) == m 

scprop_ResultMonadAssociativity :: Result Int -> Result Int -> Result Int -> Bool
scprop_ResultMonadAssociativity m k h = (m >>= (\x -> (\y -> fmap (+y) k) x >>= (\y -> fmap (+y) h)))
                                     == ((m >>= (\y -> fmap (+y) k)) >>= (\y -> fmap (+y) h))
