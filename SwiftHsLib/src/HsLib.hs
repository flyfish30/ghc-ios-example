{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module HsLib (chello) where

-- import Development.GitRev (gitHash)
import Foreign.C (CString, newCString)
import Data.Maybe
import Network.Socket
import Remote.Slave
import System.IO.Unsafe
import Control.Concurrent
import Control.Concurrent.MVar

-- | export haskell function @chello@ as @hello@.
foreign export ccall "hello" chello :: IO CString

-- | Tiny wrapper to return a CString
-- chello = newCString (hello ++ "\nfirst 20 primes: \n" ++ show primes20)
chello = do
  mend <- newEmptyMVar
  tid <- forkIO $ doSocketDemo mend
  putStrLn $ "tid = " ++ show tid
  -- docroot <- newCString "/tmp"
  -- _ <- forkIO $ startSlave True 5000 docroot
  putStrLn "lunched the socket server"
  v <- takeMVar mend
  putStrLn $ "Socket server return " ++ show v
  newCString $ hello ++ " socket opened"

-- | Pristine haskell function.
hello = "Hello from Haskell"

-- welcom = "Wecom to iOS ghc\nversion: " ++ $gitHash

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

doSocketDemo mend = do
  let portNum = 5000
  sock <- openSocket (toEnum 5000)
  putStrLn "Opening socket"
  sockn <- acceptSocket sock
  putStrLn $ "Listening on port " ++ show portNum
  doChat sockn
  close sockn
  putMVar mend 1
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
