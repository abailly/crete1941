import Loader
import System
import qualified Data.Map as M
import Data.List(isSuffixOf)

main = do (timeout:root:main:rest) <- getArgs
          let c = mkReloaderConfig excludedFiles main root rest 
          killAndRelaunch c {reloadDelay = read timeout } Nothing
            where
              excludedFiles f = not $ isSuffixOf ".git" f

          
