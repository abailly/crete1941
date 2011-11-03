-- |Provides functions for checking whether or not some directories' content has changed.
module Loader.FilesMonitor where
import qualified Data.Map as M
import System.Time
import System.Directory
import System.FilePath

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

