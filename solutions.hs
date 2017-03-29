-- PROBLEM 1

-- O(n)
problem1 :: Integer -> Integer
problem1 n = sum [d | d <- [1..n-1], d `mod` 3 == 0 || d `mod` 5 == 0]

-- Sum of positive integers divisible by d smaller than n
divisorsSum :: Integer -> Integer -> Integer
divisorsSum n d = d * (l + 1) * l `div` 2
                  where l = (n - 1) `div` d

-- O(1)
problem1' :: Integer -> Integer
problem1' n = divisors 3 + divisors 5 - divisors 15
              where divisors = divisorsSum n


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
problem2 :: Integer -> Integer
problem2 n = sum (takeWhile (<= n) (filter even fibs''))

-- Stream of even Fibonacci numbers (every 3rd): 2, 8, 34, 144, ...
-- O(n)
evenFibs :: [Integer]
evenFibs = map fst (iterate (\(a, b) -> (b, 4*b + a)) (2, 8))

-- O(n)
problem2' :: Integer -> Integer
problem2' n = sum (takeWhile (<= n) evenFibs)


-- PROBLEM 3

-- O(sqrt n)
-- After d = 2, increasing d by 2 instead of 1 is possible.
-- I liked the code brevity without this though.
-- It also doesn't change the complexity.
problem3 :: Integer -> Integer
problem3 n = divide n 2
             where divide n d | n <= 1                             = error "n <= 1 has no prime factors"
                              | n == d                             = d
                              | n `mod` d == 0                     = divide (n `div` d) d
                              | floor (sqrt (fromIntegral n)) <= d = n
                              | otherwise                          = divide n (d + 1)


-- PROBLEM 4

-- List of digits of given positive number
-- O(log n)
digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

-- O(log n)
isPalindrome :: Integer -> Bool
isPalindrome n = digits n == reverse (digits n)

-- O(n^2 * log n), where n = 100 (which is equal to length [900..999]).
-- This creates a list of (n^2 / 2) products, each tested with isPalindrome,
-- which is O(log n^2) = O(log n), i.e. linear in regards to the number of product digits.
problem4 :: Integer
problem4 = maximum (filter isPalindrome [a * b | a <- [900..999], b <- [a..999]])
