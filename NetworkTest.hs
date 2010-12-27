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
          putStrLn $ "listeing " ++ (show args)
          writeFile "out" (show args)
|]

processesCommunication = 
  "Given 1 supervisor and 1 supervised process, supervisor" `should` [
  "prepend socket number to bind to supervised process" `for`
  do supervisedProcessSetup
     root <- tempDir
     let testConfig = mkReloaderConfig (\ _ -> True) "listen" root [] 2
     killAndRelaunch testConfig Nothing
     readFile (root </> "out")
  >>= assertEqual "Expected port number for binding new process" ["1234"] . read
  ]