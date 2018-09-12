module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 35 mins
sum_primes :: [Integer] -> Integer
sum_primes xs = sum (take 100 xs)

lowest_prime :: [Integer] -> Integer
lowest_prime (x:xs) | prime ((sum_primes xs) + x) = x
                    | otherwise                   = lowest_prime xs

-- Answer
-- By checking if the sequence of primes are all prima and that the sum of them
-- is prime aswell.
