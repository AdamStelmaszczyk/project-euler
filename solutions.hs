-- O(n)
problem_1 :: Integer -> Integer
problem_1 m = sum [n | n <- [1..m-1], n `mod` 3 == 0 || n `mod` 5 == 0]

-- Sum of positive integers divisible by d smaller than m
divisors_sum :: Integer -> Integer -> Integer
divisors_sum m d = d * (l + 1) * l `div` 2
  where l = (m - 1) `div` d

-- O(1)
problem_1' :: Integer -> Integer
problem_1' m = divisors 3 + divisors 5 - divisors 15
  where divisors = divisors_sum m
