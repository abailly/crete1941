module AllTests where
import PiecesTest
import CliTest
import IoTest
import ServerTest
import LoaderTest
import NetworkTest
import IO(stderr)
import Test.HUnit
import System.Exit
import System.IO
import Data.List

newtype Tests = T {unT :: Test}

data TestCount = TestCount Int Test

tests = T $ test [
  unitManipulations
  ,movementRules
  ,combatRules
  ,combatEffect
  ,commandsHandling
  ,decodeCommandsFromStrings
  ,programLoader
  ,processesCommunication
  ]
        
testsWithServer = T $ test [
  interactThroughAnHttpServer
  ]

runAllTests tests = do putStrLn "Running test suite: "
                       putStrLn (show tests)
                       counts <- runTest (unT tests)
                       case (errors counts + failures counts) of
                         0 -> return ExitSuccess
                         n -> return (ExitFailure n)

instance Show Tests where
  show t = show' "" t
  
show'  indent (T (TestCase _))    = ""
show'  indent (T (TestList ts))   = concat $ (map (show' (' ':indent) . T) ts)
show'  indent (T (TestLabel l t)) = indent ++ l ++ ":\n" ++ (show' indent (T t))

runTest :: Test -> IO Counts
runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
		return counts



