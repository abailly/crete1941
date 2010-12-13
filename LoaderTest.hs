module LoaderTest where
import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.FilePath
import System.IO
import IO(bracket)
import qualified Data.Map as M
import Control.Monad(when)
import Loader
import TestUtilities(for,should,with, given)


tempDir = getTemporaryDirectory >>= return . (</> "loader-test")

-- |Remove temporary test directory
deleteIfExists :: FilePath -> IO ()
deleteIfExists path = do exists <- doesDirectoryExist path
                         when exists (removeDirectoryRecursive path)

createTempDir = tempDir >>= createDirectory 

-- | Setup a FS for scanning changes
simpleFileSetup :: IO ()
simpleFileSetup = do 
  tmp <- tempDir 
  deleteIfExists tmp
  createTempDir 
  writeFile (tmp </> "aFile.txt")  "this is a test" 

complexFileSetup :: IO ()
complexFileSetup = do 
  tmp <- tempDir 
  deleteIfExists tmp
  createTempDir 
  writeFile (tmp </> "aFile.txt")  "this is a test" 
  writeFile (tmp </> "bFile.txt")  "this is a test" 

programLoader = test [
  "When loader starts it" `should` [
  "display all files as added" `for`
  do simpleFileSetup
     root <- tempDir
     checkChanges [root] M.empty
  >>= (\r -> assertEqual "expected one file added" "aFile.txt" ((snd.splitFileName.fromEdit.head.fst) r))
  ],
  "when loader is triggered it" `should` [
    "mark changed files as modified" `for`
    do complexFileSetup
       root <- tempDir
       state <- checkChanges [root] M.empty
       writeFile (root </> "aFile.txt")  "this is another test"        
       checkChanges [root] (snd state)
    >>= (\r -> assertEqual "there should be 1 changed file in 2 files" 1 ((length.modified.fst) r))
    ]
  
  ]