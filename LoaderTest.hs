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
  let sub = (tmp </> "subdir")
  createDirectory sub
  writeFile (sub </> "cFile.txt")  "this is a toast"

programLoader = test [
  "When loader starts it" `should` [
  "displays all files as added" `for`
  do simpleFileSetup
     root <- tempDir
     checkChanges [root] M.empty
  >>= (\r -> assertEqual "expected one file added" "aFile.txt" ((snd.splitFileName.fromEdit.head.fst) r))
  ],
  "when loader is triggered it" `should` [
    "marks changed files as modified" `for`
    do complexFileSetup
       root <- tempDir
       state <- checkChanges [root] M.empty
       writeFile (root </> "aFile.txt")  "this is another test"        
       checkChanges [root] (snd state)
    >>= (\r -> assertEqual "there should be 1 changed file in 3 files" 1 ((length.modified.fst) r)),
    "recursively marks changed files as modified" `for` 
    do complexFileSetup
       root <- tempDir
       state <- checkChanges [root] M.empty
       writeFile (root </> "subdir" </> "cFile.txt")  "this is another toast"        
       checkChanges [root] (snd state)
    >>= (\r -> assertEqual "there should be 1 changed file in 3 files" 1 ((length.modified.fst) r)),
    "recursively finds deleted files" `for` 
    do complexFileSetup
       root <- tempDir
       state <- checkChanges [root] M.empty
       removeFile (root </> "subdir" </> "cFile.txt")
       checkChanges [root] (snd state)
    >>= (\(l,m) -> assertEqual "there should be 1 deleted file in 3 files" 1 ((length.deleted) l))
    ]
  ]