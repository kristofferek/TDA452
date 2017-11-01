import Test.QuickCheck
-- Implementation of the power function from the lecture
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

{- Part 1
  In order to calculate n^k, k+1 computing "steps" are used.
  Answer: k+1 computing "steps"
-}

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product [n | a <- [1..k]]

prop_power1 :: Integer -> Integer -> Bool
prop_power1 n k = power1 n k' == n^k'
  where k' = abs k
