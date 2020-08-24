module LoaderTest where
import           Control.Concurrent  (threadDelay)
import           Control.Monad       (when)
import           Data.List           (isSuffixOf)
import qualified Data.Map            as M
import           Loader
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           Test.HUnit
import           Test.QuickCheck
import           TestUtilities       (for, given, should, with)

import           Loader.FilesMonitor

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

aFile tmp = writeFile (tmp </> "aFile.txt")  "this is a test"
bFile tmp = writeFile (tmp </> "bFile.txt")  "this is a test"

-- | Setup a FS for scanning changes
simpleFileSetup :: IO ()
simpleFileSetup = do
  tmp <- prepareTempDir
  aFile tmp

multiExtensionFileSetup :: IO ()
multiExtensionFileSetup = do
  tmp <- prepareTempDir
  aFile tmp
  writeFile (tmp </> "aFile.html")  "this is a test"

complexFileSetup :: IO ()
complexFileSetup = do
  tmp <- prepareTempDir
  aFile tmp
  bFile tmp
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
excludeDotHtml = not . isSuffixOf ".html"

true x = True

testConfig root = mkReloaderConfig true "SomeModule.SomeApp" root [] 10 defaultLoaderPort

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
       threadDelay 1000000
       writeFile (root </> "aFile.txt")  "this is another test"
       checkChanges true [root] (snd state)
    >>= (\r -> assertEqual "there should be 1 changed file in 3 files" 1 ((length.modified.fst) r)),
    "recursively marks changed files as modified" `for`
    do complexFileSetup
       root <- tempDir
       state <- checkChanges true [root] M.empty
       threadDelay 1000000
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
       (Just out,_) <- recompile $ testConfig root
       return out
    >>= \(ex,s) -> assertEqual "build should succeed" ExitSuccess ex,
    "does not recompile main file if nothing changes" `for`
    do sourceTreeSetup
       root <- tempDir
       let c = testConfig root
       -- we need to do it 3 times because after first compilation, new files are
       -- created (.o and .hi) which are detected as changes
       (_,s) <- recompile c
       (_,s') <- recompile c {scanStatus = s}
       recompile  c {scanStatus = s'}
    >>= \(ex,s) -> assertEqual "nothing should be built" Nothing ex
    ]

  -- supervisor should be stopped if run when start
  -- stop supervisor when stopping
  ]
