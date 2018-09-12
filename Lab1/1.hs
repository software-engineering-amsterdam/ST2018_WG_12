module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 15 mins

-- Workshop 1. ex 2.
quad', fun2 :: Int -> Int
quad' n = sum (map (^2) [1..n])
fun2 n = (product [n, n + 1, 2*n + 1]) `div` 6

q_test2 = quickCheckResult (\n -> n >= 0 --> quad' n == fun2 n)

-- 15 mins

-- Workshop 1. ex 3.
three', fun3 :: Int -> Int
three' n = sum (map (^3) [1..n])
fun3 n = ((n * (n + 1)) `div` 2)^2

q_test3 = quickCheckResult (\n -> n >= 0 --> three' n == fun3 n)
