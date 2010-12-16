module LoaderTest where
import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.FilePath
import System.IO
import System.Exit
import IO(bracket)
import qualified Data.Map as M
import Control.Monad(when)
import Loader
import TestUtilities(for,should,with, given)
import Data.List(isSuffixOf)

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

-- | Setup a FS for scanning changes
simpleFileSetup :: IO ()
simpleFileSetup = do 
  tmp <- prepareTempDir 
  writeFile (tmp </> "aFile.txt")  "this is a test" 

multiExtensionFileSetup :: IO ()
multiExtensionFileSetup = do 
  tmp <- prepareTempDir 
  writeFile (tmp </> "aFile.txt")  "this is a test" 
  writeFile (tmp </> "aFile.html")  "this is a test" 

complexFileSetup :: IO ()
complexFileSetup = do 
  tmp <- prepareTempDir
  writeFile (tmp </> "aFile.txt")  "this is a test" 
  writeFile (tmp </> "bFile.txt")  "this is a test" 
  let sub = (tmp </> "subdir")
  createDirectory sub
  writeFile (sub </> "cFile.txt")  "this is a toast"

sourceTreeSetup :: IO ()
sourceTreeSetup = do
  tmp <- prepareTempDir 
  let sub = (tmp </> "SomeModule")
  createDirectory sub
  writeFile (sub </> "SomeApp.hs")  "module SomeModule.SomeApp where { import SomeModule.SomeLib; main = putStrLn toto }" 
  writeFile (sub </> "SomeLib.hs")  "module SomeModule.SomeLib where toto = \"tata\"" 
  
excludeDotHtml :: FilePath -> Bool
excludeDotHtml = isSuffixOf ".html"

true x = True

programLoader = test [
  "When loader starts it" `should` [
  "displays all files as added" `for`
  do simpleFileSetup
     root <- tempDir
     checkChanges true [root] M.empty
  >>= (\r -> assertEqual "expected one file added" "aFile.txt" ((snd.splitFileName.fromEdit.head.fst) r))
  ,
  "filters files" `for`
  do multiExtensionFileSetup
     root <- tempDir
     checkChanges excludeDotHtml [root] M.empty
  >>= (\r -> assertEqual "expected one file added" "aFile.txt" ((snd.splitFileName.fromEdit.head.fst) r))
  ],
  "when loader is triggered it" `should` [
    "marks changed files as modified" `for`
    do complexFileSetup
       root <- tempDir
       state <- checkChanges true [root] M.empty
       writeFile (root </> "aFile.txt")  "this is another test"        
       checkChanges true [root] (snd state)
    >>= (\r -> assertEqual "there should be 1 changed file in 3 files" 1 ((length.modified.fst) r)),
    "recursively marks changed files as modified" `for` 
    do complexFileSetup
       root <- tempDir
       state <- checkChanges true [root] M.empty
       writeFile (root </> "subdir" </> "cFile.txt")  "this is another toast"        
       checkChanges true [root] (snd state)
    >>= (\r -> assertEqual "there should be 1 changed file in 3 files" 1 ((length.modified.fst) r)),
    "recursively finds deleted files" `for` 
    do complexFileSetup
       root <- tempDir
       state <- checkChanges true [root] M.empty
       removeFile (root </> "subdir" </> "cFile.txt")
       checkChanges true [root] (snd state)
    >>= (\(l,m) -> assertEqual "there should be 1 deleted file in 3 files" 1 ((length.deleted) l))
    ],
  "when monitoring a directory it" `should` [
    "compiles main file" `for`
    do sourceTreeSetup
       root <- tempDir
       (Just out,_) <- recompile true M.empty "SomeModule.SomeApp" root
       return out
    >>= \(ex,s) -> assertEqual "build should succeed" ExitSuccess ex,
    "does not recompile main file if nothing changes" `for`
    do sourceTreeSetup
       root <- tempDir
       -- we need to do it 3 times because after first compilation, new files are 
       -- created (.o and .hi) which are detected as changes
       (_,s) <- recompile true M.empty "SomeModule.SomeApp" root
       (_,s') <- recompile true s "SomeModule.SomeApp" root
       recompile true s' "SomeModule.SomeApp" root
    >>= \(ex,s) -> assertEqual "nothing should be built" Nothing ex

    ]
  ]