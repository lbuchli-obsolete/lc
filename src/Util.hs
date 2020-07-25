module Util where

import Control.Applicative

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
hAlloc (Heap size (next:free) cts) node = (Heap (size+1) free ((next, node):cts), next)
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
  fmap f (Success a) = Success (f a)
  fmap f (Trace _ r) = fmap f r
  fmap _ (Error msg) = Error msg

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
