import Test.QuickCheck

power2 :: Integer -> Integer -> Integer
power2 n k | ((k `mod` 2 == 0) && (k > 2)) = power2 n (k/2)
power2 n k | k `mod` 2== 1 = n * (power2 n (k-1))
power2 n k | k == 2 = n*n

prop_power2 :: Integer -> Integer -> Bool
prop_power2 n k = power2 n k' == n^k'
  where k' = abs k
