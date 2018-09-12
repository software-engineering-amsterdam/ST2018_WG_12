module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 30 mins

-- Workshop 4
card' :: Int -> Int
card' n = length (subsequences [0..n])

pow' :: Int -> Int
pow' n = 2^(length [0..n])

q_test3 = quickCheckResult (\n -> n >= 0 --> card' n == pow' n)

-- Answer 1:
-- The subsequence increase exponantially, therefore when the length limit is
-- increased, it gets more intensive to test.
