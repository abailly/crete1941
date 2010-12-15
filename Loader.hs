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
import Debug.Trace(trace)

main = do args <- getArgs
          checkChanges ["."] M.empty
          
-- |Stores last modified timestamp of the file
type HashMap = M.Map FilePath ClockTime

data (Show a, Eq a) => Edit a = Mod a
                              | Add a
                              | Del a
                      deriving (Eq,Show)
                     
fromEdit :: (Eq a,Show a) => Edit a -> a
fromEdit (Add e) = e
fromEdit (Mod e) = e
formEdit (Del e) = e

-- |Expects as argument a list of file path roots to scan for changes,
--  returns the list of files changed.
checkChanges :: [String] -> HashMap -> IO ([Edit FilePath], HashMap)
checkChanges [fs] m = do 
    addedFilesList <- lsRecursive fs 
    timestamps <- mapM getModificationTime addedFilesList
    let allts = zip addedFilesList timestamps
    let ret   = (findDeletedFiles allts.updateScannedFiles allts) ([],m)
    return ret
    
-- | Returns an updated map and a list of modified/added/deleted files
updateScannedFiles :: [(FilePath,ClockTime)] -> ([Edit FilePath], HashMap) -> ([Edit FilePath], HashMap)
updateScannedFiles []                r           = r
updateScannedFiles ((path,ts):files) (updates,m) = 
  case M.lookup path m of
    Nothing  -> updateScannedFiles files ((Add path:updates), M.insert path ts m)
    Just ts' -> if ts' < ts then
                  updateScannedFiles files ((Mod path:updates), M.adjust (const ts) path m)
                else
                  updateScannedFiles files (updates, m)
  
ls dir          = do flg <- doesDirectoryExist dir
                     if flg then getDirectoryContents dir else return []
      
findDeletedFiles :: [(FilePath,ClockTime)] -> ([Edit FilePath], HashMap) -> ([Edit FilePath], HashMap)
findDeletedFiles files (up,m) = 
  (up ++ map Del (M.keys $ deleted),M.difference m deleted)
    where deleted = M.difference m (M.fromList files)
          
lsRecursive :: FilePath -> IO [FilePath]
lsRecursive dir = do subs <- ls dir
                     (files, dirs) <- partitionM (doesFileExist) (map (dir </>) (filter (\x -> (x /= ".") && (x /= "..")) subs))
                     subfiles <- mapM lsRecursive (dirs)
                     return $ map (dir </>) files ++ concat subfiles

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _    []     = return ([],[])
partitionM pred (x:xs) = do flag <- pred x 
                            (ins,outs) <- partitionM pred xs
                            return (if flag then (x:ins,outs) else (ins,x:outs))

modified :: [Edit FilePath] -> [FilePath]
modified (Mod f:files) = f:modified files
modified [] = []

deleted  :: [Edit FilePath] -> [FilePath]
deleted [] = []
deleted  (Del f:files) = f:deleted files

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

