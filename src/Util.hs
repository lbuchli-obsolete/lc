{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Util where

import Control.Applicative
import System.Random
import Test.SmallCheck.Series

---------------------------------------------------------
--                         Heap                        --
---------------------------------------------------------

data Heap a = Heap {
  hSize :: Addr,
  hAddrsLeft :: [Addr],
  hAllocated :: Map Addr a
}

type Addr = Int
  
hInitial   :: Heap a
hInitial = Heap 0 [1..] []

hAlloc     :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (n:free) cts) node = (Heap (size+1) free ((n, node):cts), n)
hAlloc _ _ = error "None of the infinite addresses are left (╯°□°)╯ ┻━┻"

hUpdate    :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free cts) addr node = Heap size free ((addr, node) : filter (\x -> fst x /= addr) cts)

hFree      :: Heap a -> Addr -> Heap a
hFree (Heap size free cts) addr = Heap (size-1) (addr:free) (filter (\x -> fst x /= addr) cts)

hLookup    :: Heap a -> Addr -> Result a
hLookup (Heap _ _ cts) = mLookup cts

hAddresses :: Heap a -> [Addr]
hAddresses (Heap _ _ cts) = [addr | (addr, _) <- cts]

instance Show a => Show (Heap a) where
  show (Heap size _ cts) = "--- Heap (" ++ show size ++ ") ---\n" ++ allocs cts
    where
      allocs ((k, a):other) = show k ++ "\t=> " ++ show a ++ "\n" ++ allocs other
      allocs []             = ""

-------------------------------------------------------------------
--                              Map                              --
-------------------------------------------------------------------

type Map a b = [(a, b)]

mLookup :: (Eq a, Show a) => Map a b -> a -> Result b
mLookup ((k, v):bs) k' | k == k' = Success v
                       | k /= k' = mLookup bs k'
mLookup m k'                     = Error (putStr $ "Can't find key " ++ show k' ++ " in map " ++ show (mDomain m))

mRevLookup :: (Eq b, Show b) => Map a b -> b -> Result a
mRevLookup = undefined

mDelete :: (Eq a, Show a) => Map a b -> a -> Result (Map a b)
mDelete ((k, _):xs) k' | k == k' = Success xs
mDelete (x:xs)      k'           = (:) x <$> mDelete xs k'
mDelete []          k'           = Error (putStr $ "Cannot delete " ++ show k' ++ " (Key not found)")

mDeleteAll :: (Eq a) => Map a b -> a -> Map a b
mDeleteAll ((a, _):xs) a' | a == a' = mDeleteAll xs a'
mDeleteAll ((a, b):xs) a'           = (a, b) : mDeleteAll xs a'
mDeleteAll []          _            = []

mHas :: (Eq a) => Map a b -> a -> Bool
mHas ((x, _):_) x' | x == x' = True
mHas (_:xs)     x'           = mHas xs x'
mHas []         _            = False

mDomain :: Map a b -> [a]
mDomain mMap = [key | (key, _) <- mMap]

mRange :: Map a b -> [b]
mRange mMap = [val | (_, val) <- mMap]

mEmpty :: Map a b
mEmpty = []

----------------------------------------------------------------------
--                              Result                              --
----------------------------------------------------------------------

data Result a = Success a | Trace String (Result a) | Error (IO ())

instance Functor Result where
  fmap f (Success a)   = Success (f a)
  fmap f (Trace msg r) = Trace msg $ fmap f r
  fmap _ (Error msg)   = Error msg

instance Applicative Result where
  pure x = Success x
  (<*>) (Success f) (Success a)  = Success (f a)
  (<*>) (Trace _ rf) ra          = rf <*> ra
  (<*>) (Error msg) _            = Error msg
  (<*>) rf          (Trace _ ra) = rf <*> ra
  (<*>) _           (Error msg)  = Error msg

instance Monad Result where
  (>>=) (Success a) f   = f a
  (>>=) (Trace msg a) f = Trace msg (a >>= f)
  (>>=) (Error msg) _   = Error msg

instance Alternative Result where
  empty = Error $ putStrLn "Empty"
  (<|>) (Trace _ a) b         = a <|> b
  (<|>) (Success a) _         = Success a
  (<|>) (Error a) (Trace _ b) = Error a <|> b
  (<|>) (Error _) (Success b) = Success b
  (<|>) (Error a) (Error _)   = Error a

instance Serial m a => Serial m (Result a) where
  series = cons1 Success
        \/ cons1 (Trace "Trace")
        \/ cons0 (Error (putStrLn "Error"))

instance Eq a => Eq (Result a) where
  (==) (Success a)     (Success b)     = a == b
  (==) (Success _)     _               = False
  (==) (Trace msg_a a) (Trace msg_b b) = msg_a == msg_b && a == b
  (==) (Trace _ _)     _               = False
  (==) (Error _)       (Error _)       = True
  (==) (Error _)       _               = False

instance Show a => Show (Result a) where
  show (Success a)   = "Success " ++ show a
  show (Trace msg a) = "Trace '" ++ msg ++ "' $ " ++ show a
  show (Error _)     = "Error"

{-
instance Arbitrary a => Arbitrary (Result a) where
  generate gen = _generate (next gen)
    where
      _generate (n, g) | even n = (Error (putStrLn "Arbitrary error"), g)
      _generate (_, g)          = first Success (generate g)
-}

type ShowTrace = Bool
printResult :: Show a => ShowTrace -> Result a -> IO ()
printResult _ (Error msg) = do
  putStr "ERROR: "
  msg
printResult _ (Success s) = print s
printResult True (Trace msg r) = do
  putStr "TRACE: "
  putStrLn msg
  printResult True r
printResult False (Trace _ r) = printResult False r

noTrace :: Result a -> Result a
noTrace (Success a) = Success a
noTrace (Trace _ a) = noTrace a
noTrace (Error msg) = Error msg

-----------------------------------------------
--                   Other                   --
-----------------------------------------------

none :: (a -> Bool) -> [a] -> Bool
none f xs = not $ any f xs

class Arbitrary a where
  generate :: StdGen -> (a, StdGen)
