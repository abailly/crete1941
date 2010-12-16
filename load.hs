import Loader
import System
import qualified Data.Map as M

main = do (root:main:rest) <- getArgs
          killAndRelaunch M.empty root main rest Nothing
       

          
