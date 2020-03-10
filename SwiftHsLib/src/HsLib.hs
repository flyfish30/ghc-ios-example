module HsLib (chello) where

import Foreign.C (CString, newCString)
import Data.Maybe
import Data.Vector as V hiding((++), take)

-- | export haskell function @chello@ as @hello@.
foreign export ccall "hello" chello :: IO CString

-- | Tiny wrapper to return a CString
chello = newCString (hello ++ "\nfirst 20 primes: \n" ++ show (vecAt 5))

-- | Pristine haskell function.
hello = "Hello from Haskell"

welcom = "Wecom to iOS ghc"

vecAt i = (V.generate (i+3) id) V.! i

primes20 = take 20 primes

primes = filterPrimes [2..]
  where filterPrimes (p:xs) = p : filterPrimes [x | x <- xs, x `mod` p /= 0]
