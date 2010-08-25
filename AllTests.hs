module Main where
import PiecesTest
import IO(stderr)
import Test.HUnit
import System.Exit
import System.IO

tests = test [movementRules]

main = do counts <- runTest tests
          case (errors counts + failures counts) of
            0 -> exitWith ExitSuccess
            n -> exitWith (ExitFailure n)

runTest :: Test -> IO Counts
runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
		return counts



