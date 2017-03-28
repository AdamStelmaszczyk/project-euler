-- PROBLEM 1

-- O(n)
problem_1 :: Integer -> Integer
problem_1 n = sum [d | d <- [1..n-1], d `mod` 3 == 0 || d `mod` 5 == 0]

-- Sum of positive integers divisible by d smaller than n
divisors_sum :: Integer -> Integer -> Integer
divisors_sum n d = d * (l + 1) * l `div` 2
  where l = (n - 1) `div` d

-- O(1)
problem_1' :: Integer -> Integer
problem_1' n = divisors 3 + divisors 5 - divisors 15
  where divisors = divisors_sum n


-- PROBLEM 2

-- n-th Fibonacci number 1-indexed: 1, 1, 2, 3, 5, 8, ...
-- O(phi^n), where phi is the golden ratio (1.618...)
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n - 2) + fib (n - 1)

-- Stream of Fibonacci numbers: 1, 1, 2, 3, 5, 8, ...
-- O(n * phi^n)
fibs :: [Integer]
fibs = map fib [1..]

-- O(n)
fibs' :: [Integer]
fibs' = 1 : 1 : zipWith (+) fibs' (tail fibs')

-- O(n)
fibs'' :: [Integer]
fibs'' = map fst (iterate (\(a, b) -> (b, a + b)) (1, 1))

-- O(n)
problem_2 :: Integer -> Integer
problem_2 n = sum (takeWhile (<= n) (filter even fibs''))

-- Stream of even Fibonacci numbers (every 3rd): 2, 8, 34, 144, ...
-- O(n)
even_fibs :: [Integer]
even_fibs = map fst (iterate (\(a, b) -> (b, 4*b + a)) (2, 8))

-- O(n)
problem_2' :: Integer -> Integer
problem_2' n = sum (takeWhile (<= n) even_fibs)
