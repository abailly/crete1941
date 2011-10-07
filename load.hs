import Loader
import System
import qualified Data.Map as M
import Data.List(isSuffixOf)

main = do (timeout:root:main:count:port:rest) <- getArgs
          let c = mkReloaderConfig excludedFiles main root rest (read count) 12346
          c' <- startReloader (read port) c
          killAndRelaunch c' {reloadDelay = read timeout} 
            where
              excludedFiles f = not $ isSuffixOf ".git" f

          
