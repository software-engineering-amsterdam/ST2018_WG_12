module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 40 mins

permu' :: Int -> Int
permu' n = length (permutations [1..n])

count_perms :: Int -> Int
count_perms 1 = 1
count_perms n = n * count_perms (n - 1)

q_test4 = quickCheckResult (\n -> n >= 0 --> permu' n == count_perms n)

-- Answer
-- Same as with the previous assignment, the permutations increase exponantially,
-- making it easy to test at the start, but more and more cpu intensive later on.
