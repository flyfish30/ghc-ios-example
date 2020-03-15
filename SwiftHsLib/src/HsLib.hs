{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module HsLib (chello) where

-- import Development.GitRev (gitHash)
import Foreign.C (CString, newCString)
import Data.Maybe
import qualified Data.Vector as V hiding((++), take)
import Network.Socket
import Remote.Slave
import System.IO.Unsafe
import Control.Concurrent
import qualified Data.ByteString.Char8 as S

-- | export haskell function @chello@ as @hello@.
foreign export ccall "hello" chello :: IO CString

-- | Tiny wrapper to return a CString
-- chello = newCString (hello ++ "\nfirst 20 primes: \n" ++ show (vecAt 5))
chello = do
  _ <- doSocketDemo
  -- docroot <- newCString "/tmp"
  -- _ <- forkIO $ startSlave True 5000 docroot
  putStrLn "lunched the socket server"
  newCString $ hello ++ " socket opened"

-- | Pristine haskell function.
hello = "Hello from Haskell"

-- welcom = "Wecom to iOS ghc\nversion: " ++ $gitHash

vecAt i = (V.generate (i+3) id) V.! i

primes20 = take 20 primes

primes = filterPrimes [2..]
  where filterPrimes (p:xs) = p : filterPrimes [x | x <- xs, x `mod` p /= 0]

--------------------------------------------------------------------------------
-- socket to pipe briding logic.
{-
openSocketS :: PortNumber -> IO Socket
openSocketS port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 1
  return sock

acceptSocketS :: Socket -> IO Socket
acceptSocketS = fmap fst . accept
-}

doSocketDemo = do
  let portNum = 5000
  sock <- openSocket (toEnum 5000)
  putStrLn "Opening socket"
  sockn <- acceptSocket sock
  putStrLn $ "Listening on port " ++ show portNum
  doChat sockn
  close sockn
  putStrLn "serv ended"
  return ()

doChat sock = do
  msg <- recv sock 1024
  case (head msg) of
    'q' -> return ()
    'h' -> do
        send sock "Supported Command: h, q, r, w, e\n"
        doChat sock
    _   -> do
        send sock "Hello from iOS haskell server\n"
        doChat sock
