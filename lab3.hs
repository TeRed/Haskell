-- Ex 1

mojeLiczby = [f x | x <- lista, p x]
             where f = \a -> 2 * a          -- f mnoży liczbę razy 2
                   lista = [1..10]          -- lista początkowa
                   p = \b -> b `mod` 2 == 0 -- p wybiera liczby parzyste
 
mojeLiczby' = map f $ filter p lista
             where f = \a -> 2 * a
                   lista = [1..10]
                   p = \b -> b `mod` 2 == 0

-- Ex 2

generatorOperator :: (lewa -> prawa -> wynik) -> lewa -> (prawa -> wynik)

generatorOperator o l = o l

-- Ex 3                   

myReverse :: String -> String
myReverse' :: String -> String

myReverse str = foldl (\a b -> [b] ++ a) "" str
myReverse' str = foldr (\a b -> b ++ [a]) [] str

-- Ex 4

policzISumuj :: (Int -> Int) -> Int -> Int -> Int

policzISumuj f a b = sum $ map f [a..b]

-- Ex 5

pierwsze :: [Int] -> [Int]

pierwsze x = filter (\a -> length [z | z <- [1..a], (a `mod` z) == 0] == 2) x

-- Ex 6

conajmniejn' :: [Int] -> Int -> [Int]

conajmniejn' t i = [x | x <- [(minimum t)..(maximum t)], (length $ filter (\a -> a == x) t) >= i]
