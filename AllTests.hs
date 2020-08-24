module AllTests where

import           CliTest
import           IoTest
import           PiecesTest
--import ServerTest
import           Data.List
import           LoaderTest
import           NetworkTest
import           System.Exit
import           System.IO
import           Test.HUnit

newtype Tests = T {unT :: Test}

data TestCount = TestCount Int Test

tests = T $ test [
  unitManipulations,
  --                 movementRules,
  --                 combatRules,
  --                 combatEffect,
  --                 commandsHandling,
  --                 decodeCommandsFromStrings,
  --                programLoader
  -- ,
                 processesCommunication
--                 ,interactThroughAnHttpServer
                 ]

runAllTests = do putStrLn "Running test suite: "
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



