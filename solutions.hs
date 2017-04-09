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


-- PROBLEM 5

-- Smallest positive number divisible by all of the numbers from 1 to 20
problem5 :: Integer
problem5 = divisible 2520
           where divisible x | and [x `mod` d == 0 | d <- [2..20]] = x
                             | otherwise                           = divisible (x + 2520)

problem5' :: Integer
problem5' = foldr lcm 1 [1..20]


-- PROBLEM 6

-- O(n)
problem6 :: Integer -> Integer
problem6 n = sum [1..n] ^ 2 - sum [i ^ 2 | i <- [1..n]]

-- O(1)
problem6' :: Integer -> Integer
problem6' n = squaresSum ^ 2 - sumSquares
              where squaresSum = n * (n + 1) `div` 2
                    sumSquares = (2 * n + 1) * (n + 1) * n `div` 6


-- PROBLEM 7

-- O(sqrt n)
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = and [n `mod` d /= 0 | d <- 2 : [3, 5..sqrtN]]
            where sqrtN = floor (sqrt (fromIntegral n))

-- Stream of prime numbers: 2, 3, 5, 7, 11, ...
-- O(n * sqrt n)
primes :: [Integer]
primes = [p | p <- [2..], isPrime p]

-- n-th prime number, 1-indexed
-- O(n * sqrt n)
prime :: Integer -> Integer
prime n = primes !! (fromIntegral n - 1)

-- O(n * sqrt n)
problem7 :: Integer -> Integer
problem7 n = prime n


-- PROBLEM 8

problem8number = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

-- List of slices of length n
-- O(n * length xs)
slices :: Integer -> [Integer] -> [[Integer]]
slices n xs | fromIntegral n > length xs = []
            | otherwise                  = [take (fromIntegral n) xs] ++ slices n (tail xs)

-- O(n * length problem8number)
problem8 :: Integer -> Integer
problem8 n = maximum (map product (slices n (digits problem8number)))
