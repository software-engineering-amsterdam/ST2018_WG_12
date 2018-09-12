module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 38 mins

reversal :: Integer -> Integer
reversal = read . reverse . show

check_inverse :: Integer -> Bool
check_inverse n = prime n && prime (reversal n)

find_rev_primes :: Integer -> [Integer]
find_rev_primes n = filter (check_inverse) [1..n]

-- Answer
-- This can be tested in the following manner:

q_test6 = quickCheckResult (\n -> prime n --> reversal (reversal n) == n)

-- Prime is checked to ensure that values like '10 && 01' aren't verified
