{-# LANGUAGE DeriveDataTypeable #-}
-- |Common types and functions used ubiquitously across the system.
module Common where
import qualified Data.Generics as G

type Name   = String

data Side = German 
          | British
            deriving (Eq, Show, Read, G.Data, G.Typeable)
                     