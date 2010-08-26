-- |Common types and functions used ubiquitously across the system.
module Common where

type Name   = String

data Side = German 
          | British
            deriving (Eq, Show, Read)
                     