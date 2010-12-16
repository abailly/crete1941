import Loader
import System
import qualified Data.Map as M
import Data.List(isSuffixOf)

main = do (root:main:rest) <- getArgs
          killAndRelaunch excludedFiles M.empty root main rest Nothing
            where
              excludedFiles f = not $ isSuffixOf ".git" f

          
