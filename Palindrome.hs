{- ©2011 - Arnaud Bailly -}
import Test.HUnit
import Data.Time.Clock(getCurrentTime,diffUTCTime)
import IO(stderr)
import System.Exit
import Debug.Trace
import Control.Arrow((***))
import Control.Parallel(par)

threeDigits = [100..999]

-- |Compute all valid divisors between 100 and 999 of a number.
divisors :: Int -> 
            [(Int,Int)] -- ^fst is result of integral division, snd is divisor
divisors n = [(n `div` i,i) | i <- threeDigits, n `mod` i == 0, n `div` i >= 100]
  
triples :: [([(Int,Int)],Int)] -> [(Int,Int,Int)]
triples nums = concatMap (\ (xs,x) -> map (\(y,z) -> (x,y,z)) xs) nums

all3Digits :: (Int,Int,Int) -> Bool
all3Digits (x,y,z) = is3Digit x && is3Digit y && is3Digit z
  where
    is3Digit x = x >= 100 && x <= 999
 
-- |Assert if a number is divisible by 3 3-digits numbers.
divisibleBy3DigitsNumber :: Int -> Bool
divisibleBy3DigitsNumber n = decomposition n /= []

-- |Decompose a number into a list of 3 3-digits divisors.
-- Returned list may be empty (number is not divisible by 3 3-digits numbers), or contains
-- duplicates.
decomposition :: Int -> [(Int,Int,Int)]
decomposition = (filter all3Digits.       -- remove all non-3-digits divisors
                 triples.                 -- normalize to a triple of numbers
                 map (divisors *** id).   -- divisors of the divisors
                 divisors)                -- compute all divisors

-- |Expand a string (which may or may not be already a palindrome) to a palindrome 
-- over all digits.
palindrome :: String -> [String]
palindrome s  = [ c ++ s ++ c | c <- (map show [0..9])]

-- |Compute list of all 9-digits palindromes between 100*100*100 and 999*999*999.
allPalindromes :: [Int]
allPalindromes = (filter divisible .                               -- divisible by 3 3-digits numbers
                  filter (\ x -> x >= 10^6 && x <= 999*999*999) .  -- filter acceptable range
                  map read .                                       -- convert to numbers
                  filter ((/= '0') . head).                        -- remove those beginning by 0
                  expand . expand . expand . expand .              -- compute all nine digits palindromes
                  map show) [0..9]
  where
    {- 
       Here we try to be clever by evaluating in parallel whether a number is divisible by 3-digits numbers,
       but this does yield any major improvement using 'par' operator. Might be better off using concurrent threads
       over the produced list of palindromes.
       Parallelization is activated by running:
       > Palindrome +RTS -Nx 
       where x is the number of cores to use.
     -}
    expand = concatMap palindrome
    divisible x = divisibleBy3DigitsNumber x `par` divisibleBy3DigitsNumber x 
    
main = do start <- getCurrentTime 
          let s = allPalindromes
          putStr $ "Computed " ++ show (length s) ++ " palindromes in "
          end <- getCurrentTime
          putStrLn $ show (end `diffUTCTime` start)
          let m = (maximum s)
          putStrLn $ "Maximum is " ++ show m ++ " (" ++ (show $ decomposition m) ++ ")"

