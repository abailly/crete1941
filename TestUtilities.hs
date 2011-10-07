module TestUtilities where
import qualified Control.Exception as E
import Test.HUnit
import Text.Regex.Posix

-- | Assertion over regular expression.
(~?~) :: String     -- ^ Actual computed value
         -> String  -- ^ Regular expression the actual value should match
         -> Assertion
actual ~?~ expected = assertBool ("expected string matching " ++ expected ++", got " ++ actual) (actual =~ expected :: Bool)

-- | Assertion over exception
(~?!) :: (E.Exception e, Eq e, Show e) 
         => IO a
         -> e
         -> Test
action ~?! exception = TestCase $ (action >> return ()) `E.catch` \ e' -> assertEqual ("expected exception " ++ (show exception)) exception e'

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

