{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Orders where
import qualified Data.Generics as G

import Terrain

class (G.Data order, G.Typeable order) => Order order where
  type Result order :: * -- ^ What this type of order produces
  execute :: (BattleMap t) => order -> Battle t (Result order)
  rollback :: (BattleMap t) => order -> Battle t ()
