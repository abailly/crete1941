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
import System.IO
import System.IO.Error(catch)
import System.Process
import System.Exit
import Control.Exception
import Control.Concurrent
import qualified Data.Map as M
import Control.Monad(filterM)
import System.Time
import Debug.Trace(trace)

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

-- |Reloader configuration and current status for monitoring one application
data HotReloader = HotReloader {
  loaderFilter    ::  (FilePath -> Bool),  -- ^Filter applied to found files, only matching files are monitored
  reloadDelay     :: Int,                  -- ^Delay between each reloading attempt, in microseconds
  rootDirectory   :: FilePath,             -- ^Root directory to monitor
  mainModule      :: String,               -- ^Main module (maybe in dotted or slash notation)
  mainArgs        :: [String],             -- ^Main program arguments
  scanStatus      :: HashMap               -- ^Status of scanned files 
  }
                    
mkReloaderConfig :: (FilePath -> Bool) -> String -> FilePath -> [String] -> 
                    HotReloader
mkReloaderConfig flt main root args = HotReloader flt 5000000 root main args M.empty

-- | Kill and relaunch given application
killAndRelaunch :: HotReloader -> Maybe ProcessHandle -> IO ()
killAndRelaunch c maybePid = do
  (what,state') <- recompile c
  let c' = c { scanStatus = state' } 
  case what of
    Nothing       -> threadDelay (reloadDelay c)  >> killAndRelaunch c' maybePid
    Just (ex,out) -> stop maybePid >> 
                     case ex of
                       ExitSuccess -> do
                         putStrLn out
                         putStrLn $ "Process " ++ (mainModule c)  ++ " succcesfully recompiled, restarting..."
                         (_, _, _, pid') <-
                           createProcess (proc (rootDirectory c </> map (replace '.' '/') (mainModule c)) (mainArgs c))
                         putStrLn $ "Process restarted"
                         killAndRelaunch c' (Just pid')
                       _           -> do 
                         putStrLn $ "Failed to recompile process " ++ (mainModule c) ++ ", giving up.\n" ++ out
                         killAndRelaunch c' Nothing
  where
    stop (Just pid) = System.IO.Error.catch (terminateProcess pid) (\ e -> putStrLn ("Cannot terminate process: " ++ (show e) ++ ", contnuing....") >> return ())
    stop Nothing    = return ()
    
replace :: (Eq a) => a -> a -> a -> a
replace from to x | x == from = to
                  | otherwise = x
  
-- | Recompile application if there is any change in the underlying
-- source files.
recompile :: HotReloader ->       -- ^Root directory to scan changes in
             IO (Maybe (ExitCode,String),HashMap) -- ^If application has been recompiled, log the result, otherwise Nothing
recompile c = do
  (changes,state') <- checkChanges (loaderFilter c) [(rootDirectory c)] (scanStatus c)
  if changes /= [] then  
    doRecompile (mainModule c) (rootDirectory c) >>= \log -> return (Just log, state')  
    else    
    return (Nothing, state')
    
doRecompile :: String -> FilePath -> IO (ExitCode,String)
doRecompile mainModule rootDirectory = do
  (_, Just outh, Just errh, pid) <-
       createProcess (proc "D:/Program Files/Haskell Platform/2010.2.0.0/bin/ghc.exe" 
                      ["--make", "-threaded", mainModule]) { cwd = Just rootDirectory,
                                                             std_out = CreatePipe, 
                                                             std_err = CreatePipe}
  outMVar <- newEmptyMVar
  
  -- fork off a thread to start consuming stdout
  out  <- hGetContents outh
  _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()
  
  err  <- hGetContents errh
  _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()
  takeMVar outMVar
  takeMVar outMVar
  hClose outh
  -- wait on the process
  ex <- waitForProcess pid
  return (ex,out ++ err)

-- |Expects as argument a list of file path roots to scan for changes,
--  returns the list of files changed.
checkChanges :: (FilePath -> Bool) ->           -- ^Filter applied to found files, only matching files are monitored
                [String] ->                     -- ^List of root directories to monitor
                HashMap ->                      -- ^Previous state of files 
                IO ([Edit FilePath], HashMap)   -- ^List of changes found and updated state
checkChanges flt [fs] m = do 
    addedFilesList <- lsRecursive flt fs 
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

findDeletedFiles :: [(FilePath,ClockTime)] -> ([Edit FilePath], HashMap) -> ([Edit FilePath], HashMap)
findDeletedFiles files (up,m) = 
  (up ++ map Del (M.keys $ deleted),M.difference m deleted)
    where deleted = M.difference m (M.fromList files)
          
ls dir          = do flg <- doesDirectoryExist dir
                     if flg then getDirectoryContents dir else return []
      
lsRecursive :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
lsRecursive flt dir = do subs <- ls dir
                         (files, dirs) <- partitionM doesFileExist (map (dir </>) (filter (\x -> (x /= ".") && (x /= "..")) subs))
                         subfiles <- mapM (lsRecursive flt) (filter flt dirs)
                         return $ filter flt files ++ concat subfiles

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

