{-# LANGUAGE FlexibleInstances #-}
module Terrain.Simple where
import Common
import Terrain
import Units

instance Terrain [(Name,[(Name,Bool)])] where
    connection t n1 n2 = (lookup n1 t >>= lookup n2)

