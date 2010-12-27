{-# LANGUAGE QuasiQuotes #-}
module NetworkTest where
import Here
import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.FilePath
import TestUtilities(for,should,with, given)
import Control.Monad(when)
import Loader
import Loader.Communication 

tempDir = getTemporaryDirectory >>= return . (</> "loader-test")

-- |Remove temporary test directory
deleteIfExists :: FilePath -> IO ()
deleteIfExists path = do exists <- doesDirectoryExist path
                         when exists (removeDirectoryRecursive path)

createTempDir = tempDir >>= createDirectory 

prepareTempDir = do 
  tmp <- tempDir 
  deleteIfExists tmp
  createTempDir 
  return tmp

supervisedProcessSetup = do
  tmp <- prepareTempDir 
  writeFile (tmp </> "listen.hs")  [$here|
import System
import System.IO
main = do args <- getArgs 
          putStrLn $ "listening " ++ (show args)
          writeFile "out" (show args)
|]

supervisedMulticastProcessSetup = do
  tmp <- prepareTempDir 
  writeFile (tmp </> "listen1.hs")  [$here|
import System
import System.IO
import Network.Socket
import Network.Multicast
main = withSocketsDo $ do 
  [port] <- getArgs 
  putStrLn $ "listening @" ++ show port
  address <- inet_addr "127.0.0.1"
  sock <- socket AF_INET Datagram defaultProtocol
  let addr = SockAddrInet ((fromIntegral 1234) :: PortNumber) address
  bindSocket sock addr
  putStrLn $ "receiving from " ++ show sock
  (msg, _, addr) <- recvFrom sock 1024
  writeFile "mcast" (show msg)
|]

processesCommunication = 
  "Given 1 supervisor and 1 supervised process, supervisor" `should` [
  "prepend socket number to bind to supervised process" `for`
  do supervisedProcessSetup
     root <- tempDir
     let testConfig = mkReloaderConfig (\ _ -> True) "listen" root [] 2
     killAndRelaunch testConfig Nothing
     readFile (root </> "out")
  >>= assertEqual "Expected port number for binding new process" ["1234"] . read,
  "send stop signal to supervised process before quitting" `for`
  do supervisedMulticastProcessSetup
     root <- tempDir
     let testConfig = mkReloaderConfig (\ _ -> True) "listen1" root [] 2
     killAndRelaunch testConfig Nothing
     readFile (root </> "mcast")
  >>= assertEqual "Expected stop signal" "stop" . read
  ]