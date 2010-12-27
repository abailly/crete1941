-- From http://groups.google.com/group/fa.haskell/browse_thread/thread/0172df171348c8bc/6a63dc4540f0486d?#6a63dc4540f0486d
module Here (here) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

here :: QuasiQuoter
here = QuasiQuoter (litE . stringL) (litP . stringL) 