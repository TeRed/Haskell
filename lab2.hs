{-# LANGUAGE ParallelListComp #-}

-- Ex 1

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sign :: Double -> Double
sign x | x > 0  = 1
       | x == 0 = 0
       | otherwise = -1

sum2a :: (Int, Int) -> Int
sum2a (m, n) = m + n

sum2b :: [Int] -> Int
sum2b (m:n:_) = m + n

sum2c :: Int -> Int -> Int
sum2c m n = m + n

-- Ex 2

zip' :: [a] -> [b] -> [(a, b)]
zip' a b = [(x,y) | x <- a | y <- b]

zip'' :: [a] -> [b] -> [(a, b)]
-- zip'' x y   |   length x == 0 || length y == 0 = []
zip'' [] [] = []
zip'' x [] = []
zip'' [] x = []
zip'' (x:xs) (y:ys) = [(x,y)] ++ (zip'' xs ys)

-- Ex 3

bakteria :: Int -> (Int, Int)
bakteria 0 = (1,1)
bakteria n = (s, f * 2 + s)
            where   arg = n - 1
                    next = bakteria arg
                    f = fst next
                    s = snd next

-- Ex 4

cyf :: Int -> Int
cyf x   | x < 10 = x
        | x >= 10 = cyf ((x `mod` 10) + cyf (x `div` 10))

-- Ex 5

deleteGiven :: Eq a => [a] -> a -> [a]
deleteGiven [] x = []
deleteGiven l x = [z | z <- l, z /= x]

usunduplikaty :: Eq a => [a] -> [a]
usunduplikaty [] = []
usunduplikaty (x:xs) = x : (usunduplikaty $ deleteGiven xs x)

-- Ex 6

argConajmniejn :: Eq a => [a] -> a -> Int -> [a]
argConajmniejn [] arg 0 = [arg]
argConajmniejn [] arg n = []
argConajmniejn l arg n  | len >= n = [arg]
                        | otherwise = [] 
                        where len = length [z | z <- l, z == arg]

conajmniejn :: Eq a => [a] -> Int -> [a]

conajmniejn [] n = []
conajmniejn (x:xs) n = (argConajmniejn xs x (n - 1)) ++ (conajmniejn (deleteGiven xs x) n)
