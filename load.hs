import Loader
import System
import qualified Data.Map as M
import Data.List(isSuffixOf)

main = do (timeout:root:main:rest) <- getArgs
          killAndRelaunch excludedFiles (read timeout :: Int) M.empty root main rest Nothing
            where
              excludedFiles f = not $ isSuffixOf ".git" f

          
