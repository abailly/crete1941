{-
A module loader inspired by John MacFarlane's work on Gitit.

This loader does the following:
 - scans the source files for potential changes,
 - compile the source files using GHC's make,
 - kill an existing instance of the program, if any, by sending a HUP
   signal. This should have the additional side-effect that the
   program saves its state,
 - restart the program by loading it through GHC in another process
   and invoking its main function.
-}

module Loader where
import System.Directory
import System.FilePath
import System
import qualified Data.Map as M
import Control.Monad(filterM)
import System.Time

main = do args <- getArgs
          checkChanges ["."] M.empty
          
-- |Stores last modified timestamp of the file
type HashMap = M.Map FilePath ClockTime

data (Eq a) => Edit a = Mod a
                      | Add a
                      | Del a
                      deriving (Eq)
                     
fromEdit :: (Eq a) => Edit a -> a
fromEdit (Add e) = e
fromEdit (Mod e) = e
formEdit (Del e) = e

-- |Expects as argument a list of file path roots to scan for changes,
--  returns the list of files changed.
checkChanges :: [String] -> HashMap -> IO ([Edit FilePath], HashMap)
checkChanges [fs] m = do 
    addedFilesList <- getDirectoryContents fs >>= filterM (doesFileExist) . map (fs </>)
    timestamps <- mapM getModificationTime addedFilesList
    let addedFilesMap = M.fromList $ zip addedFilesList timestamps
    return (map Add addedFilesList,addedFilesMap)

modified :: [Edit FilePath] -> [FilePath]
modified _ = []

-- import Network.Gitit.Types
-- import System.FilePath
-- import Control.Monad (unless)
-- import System.Log.Logger (logM, Priority(..))
-- import Data.List (isInfixOf, isPrefixOf)
-- import GHC
-- import GHC.Paths
-- import Unsafe.Coerce

-- loadPlugin :: FilePath -> IO Plugin
-- loadPlugin pluginName = do
--   logM "gitit" WARNING ("Loading plugin '" ++ pluginName ++ "'...")
--   runGhc (Just libdir) $ do
--     dflags <- getSessionDynFlags
--     setSessionDynFlags dflags
--     defaultCleanupHandler dflags $ do
--       -- initDynFlags
--       unless ("Network.Gitit.Plugin." `isPrefixOf` pluginName)
--         $ do
--             addTarget =<< guessTarget pluginName Nothing
--             r <- load LoadAllTargets
--             case r of
--               Failed -> error $ "Error loading plugin: " ++ pluginName
--               Succeeded -> return ()
--       let modName =
--             if "Network.Gitit.Plugin" `isPrefixOf` pluginName
--                then pluginName
--                else if "Network/Gitit/Plugin/" `isInfixOf` pluginName
--                        then "Network.Gitit.Plugin." ++ takeBaseName pluginName
--                        else takeBaseName pluginName
--       pr <- findModule (mkModuleName "Prelude") Nothing
--       i <- findModule (mkModuleName "Network.Gitit.Interface") Nothing
--       m <- findModule (mkModuleName modName) Nothing
--       setContext [] [m, i, pr]
--       value <- compileExpr (modName ++ ".plugin :: Plugin")
--       let value' = (unsafeCoerce value) :: Plugin
--       return value'


-- loadPlugin :: FilePath -> IO Plugin
-- loadPlugin pluginName = do
--   error $ "Cannot load plugin '" ++ pluginName ++
--           "'. gitit was not compiled with plugin support."
--   return undefined


-- loadPlugins :: [FilePath] -> IO [Plugin]
-- loadPlugins pluginNames = do
--   plugins' <- mapM loadPlugin pluginNames
--   unless (null pluginNames) $ logM "gitit" WARNING "Finished loading plugins."
--   return plugins'

