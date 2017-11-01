import Test.QuickCheck

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

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k == 0            = 1
           | k == 1            = n
           | k `mod` 2 == 0    = power2 (n*n) (k `div` 2)
           | k `mod` 2 == 1    = n * (power2 n (k-1))

prop_power2 :: Integer -> Integer -> Bool
prop_power2 n k = power2 n k' == n^k'
  where k' = abs k

-- Part 4

prop_power :: Integer -> Integer -> Bool
prop_power n k = (power1 n k' == power2 n k') && (power1 n k' == n^k')
  where k' = abs k
{-
Instead of comming up with our own test cases we use QuickCheck.
QuickCheck works becasue we have added "k' = abs k", which makes sure that we
doesn't test for negative values of k.
-}
