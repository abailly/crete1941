module TestUtilities where
import Test.HUnit

-- | test a list of assertions
shouldBe n = (n ~:) . TestList  . map TestCase 

-- | test a list of Test objects
should   n = (n ~:) . TestList  
given      = should

-- | name a test
for      n = (n ~:)
when     n  =(n ~:)

-- | to be used as copule with shouldBe.
-- Can be used to produce a list of assertions from a 
-- a function and a list of parameters
with       = map

infixl 0 `for`
infixl 0 `when`
infixl 0 `shouldBe`

