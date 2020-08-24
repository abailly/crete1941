{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
module NetworkTest where
import           Control.Concurrent   (takeMVar, threadDelay)
import           Control.Exception    (try)
import           Control.Monad        (when)
import           Debug.Trace
import           Here
import           System.Directory
import           System.FilePath
import           System.IO            (IOMode (ReadMode), hGetContents,
                                       withFile)
import           Test.HUnit
import           Test.QuickCheck
import           TestUtilities        (for, given, should, with)
import           Text.Regex.Posix

import           Loader.Builder
import           Loader.Communication

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
  writeFile (tmp </> "listen.hs")  [here|
import System
import System.IO
main = do args <- getArgs
          writeFile "out" (show args)
|]
  doRecompile "listen" tmp

supervisedMulticastProcessSetup = do
  tmp <- prepareTempDir
  writeFile (tmp </> "listen1.hs")  [here|
import System
import System.IO
import Network.Socket

main = withSocketsDo $ do
  [host,port] <- getArgs
  -- remote host address
  address <- inet_addr host
  let addr = SockAddrInet ((fromIntegral$read port) :: PortNumber) address
  -- own host address
  sock <- socket AF_INET Datagram defaultProtocol
  let ownAddr = SockAddrInet 0 address
  bindSocket sock ownAddr
  ownPort <- socketPort sock
  sendTo sock (show ["127.0.0.1", show ownPort]) addr
|]
  doRecompile "listen1" tmp

waitForFile file = waitForFile' file 100
  where
    waitForFile' file 0 = doesFileExist file
    waitForFile' file n = do fileExist <- doesFileExist file
                             if fileExist then return True else (threadDelay 200000 >> waitForFile' file (n-1))

waitForFileContentMatching file regexp = do waitForFile file
                                            waitForContent 100
  where
    waitForContent 0 = return False
    waitForContent n = do s <- readFile file
                          if (s =~ regexp :: Bool) then return True else (threadDelay 200000 >> waitForContent (n-1))

monitor :: FilePath -> Supervised
monitor path = Supervised "toto" path "listen" [] Nothing Nothing Nothing

processesCommunication =
  "Given 1 monitor and 1 supervised process, monitor" `should` [

  "prepend own host:port to supervised process" `for`
  do supervisedProcessSetup
     root <- tempDir
     sup <- supervisor 13570 "supervisor.log"
     (_, sup') <- supervise (monitor root) sup
     let file = root </> "out"
     waitForFile file
     stopSupervisor sup
     readFile (file)
  >>= assertEqual "Expected port number for supervisor process" ["127.0.0.1", show 13570] . read,

  "supervised process send own host:port to monitor upon startup" `for`
  do supervisedMulticastProcessSetup
     root <- tempDir
     sup <- supervisor 13571 (root </> "supervisor1.log")
     (sup',_) <- supervise ((monitor root) { mainModule ="listen1"}) sup
     let file = root </> "supervisor1.log"
     found <- waitForFileContentMatching file ".*[\"127.0.0 .1\",\"[0-9]+\"].*"
     stopSupervisor sup'
     sup'' <- takeMVar (termination sup')
     putStrLn $ show sup''
     readFile file
  >>= (assertStringMatch "Expected supervised process information signal"  ".*[\"127.0.0 .1\",\"[0-9]+\"].*") . (\s -> trace s s) . map (replace '\n' ' ')
  ]

-- stop a non existing process
-- stop correctly a supervisor
-- stop a non running supervisor
-- socket is not free when starting supervisor
-- supervised is removed when stopped
-- supervised acknoweldged when stopped
-- supervisor ping supervised

assertStringMatch :: String -> String -> String -> Assertion
assertStringMatch msg pat s =  assertBool msg  (s =~ pat :: Bool)
