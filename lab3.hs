mojeLiczby = [f x | x <- lista, p x]
             where f = \a -> 2 * a          -- f mnoży liczbę razy 2
                   lista = [1..10]          -- lista początkowa
                   p = \b -> b `mod` 2 == 0 -- p wybiera liczby parzyste
 
mojeLiczby' = map f $ filter p lista -- map f (filter p lista)
             where f = \a -> 2 * a
                   lista = [1..10]
                   p = \b -> b `mod` 2 == 0

reverse' :: String -> String

reverse' str = foldl (\a b -> [b] ++ a) "" str
reverse2' str = foldr (\a b -> b ++ [a]) [] str

policzISumuj :: (Int -> Int) -> Int -> Int -> Int

policzISumuj f a b = sum $ map f [a..b]

pierwsze :: [Int] -> [Int]

pierwsze a = filter (\a -> length [x | x <- [1..a], (a `mod` x) == 0] == 2) a

generatorOperator :: (lewa -> prawa -> wynik) -> lewa -> (prawa -> wynik)

generatorOperator o l = o l

conajmniejn' :: [Int] -> Int -> [Int]

conajmniejn' t i = [x | x <- [(minimum t)..(maximum t)], (length $ filter (\a -> a == x) t) >= i]
