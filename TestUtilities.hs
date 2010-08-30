module TestUtilities where
import Test.HUnit

shouldBe n = (n ~:) . TestList  . map TestCase 
should   n = (n ~:) . TestList  
for      n = (n ~:)
with       = map

infixl 0 `for`
infixl 0 `shouldBe`

