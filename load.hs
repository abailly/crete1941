import Loader
import System
import qualified Data.Map as M
import Data.List(isSuffixOf)

main = do (timeout:root:main:count:rest) <- getArgs
          let c = mkReloaderConfig excludedFiles main root rest (read count) 
          killAndRelaunch c {reloadDelay = read timeout } Nothing
            where
              excludedFiles f = not $ isSuffixOf ".git" f

          
