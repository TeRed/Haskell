{-# LANGUAGE FlexibleInstances #-}

-- IMIĘ I NAZWISKO: _________________

-----------------------------------------------------------------------------

-- SEKCJA 1: PROSTE LISTY NA ROZGRZEWKĘ

-- [7.5%] ZADANIE 1.A
-- Napisz funkcję, która wyciąga elementy znajdujące się na parzystych
-- pozycjach w zadanej liście
-- Przykład:
-- ghci> parzyste [2, 5, 6, 13, 32]
-- [5, 13]
parzyste :: [Int] -> [Int]
-- parzyste = undefined

parzyste [] = []
parzyste (x:y:xs) = y : parzyste xs
parzyste [x] = []

--------------------------------------

-- [7.5%] ZADANIE 1.B
-- Napisz funkcję, która przyjmuje dwa argumenty: START oraz ILE
-- i zwraca listę zawierającą kwadraty kolejnych liczb całkowitych
-- poczynając od liczby START. Lista powinna zawierać ILE liczb
-- Przykład:
-- ghci> kwadraty 3 4
-- [9, 16, 25, 36]
-- ghci> kwadraty 0 2
-- [1, 4]
-- ghci> kwadraty 2 0
-- []
kwadraty :: Int -> Int -> [Int]
-- kwadraty = undefined

kwadraty start ile = [x*x | x <- [start .. end]] where end = start + ile - 1

-----------------------------------------------------------------------------

-- SEKCJA 2: MAP

-- [15%] Zdefiniuj rekurencyjną funkcję podwojnaMapa, która przyjmuje dwie
-- funkcje oraz listę i aplikuje funkcję pierwszą do elementów nieparzystych,
-- a funkcję drugą do parzystych elementów listy
-- Przykład:
-- ghci> podwojnaMapa (+10) (+100) [1, 2, 3, 4, 5]
-- [11, 102, 13, 104, 15]
podwojnaMapa :: (a -> b) -> (a -> b) -> [a] -> [b]
-- podwojnaMapa = undefined

podwojnaMapa _ _ [] = []
podwojnaMapa f1 f2 (x:y:xs) = (f1 x) : (f2 y) : podwojnaMapa f1 f2 xs
podwojnaMapa f1 _ [x] = [f1 x]

-----------------------------------------------------------------------------

-- SEKCJA 3: NATURALNA

-- Dany jest typ reprezentujący liczby naturalne:
data Naturalna = Zero | Kolejna Naturalna
    deriving Show
-- (definiowane są jako zero albo jako coś co jest kolejną liczbą po innej
-- liczbie naturalnej)


-- [5%] ZADANIE 3.1
-- Zdefiniuj funkcję, która transformuje daną liczbę typu Naturalna na
-- odpowiadającego jej Inta
-- Przykład:
-- ghci> naturalnaToInt (Kolejna (Kolejna (Kolejna Zero)))
-- 3
naturalnaToInt :: Naturalna -> Int
-- naturalnaToInt = undefined

naturalnaToInt Zero = 0
naturalnaToInt (Kolejna x) = 1 + naturalnaToInt x

--------------------------------------

-- [10%] ZADANIE 3.2
-- Chcemy również dokonywać konwersji w drugą stronę: z dowolnego typu
-- na typ Naturalna. W tym celu:
-- (a) stwórz type-class (a.k.a. interfejs) Naturalable (oznaczający,
-- że coś da się zamienić na typ Naturalna) definiujący funkcję toNaturalna,
-- która dokonuje właściwej konwersji
-- (b) zdefiniuj taką konwersję (z wykorzystaniem Naturalable) dla typu Int
-- Przykład (musimy jawnie podać typ Int!):
-- ghci> toNaturalna (3 :: Int)
-- Kolejna (Kolejna (Kolejna Zero))

class Naturalable a where
    toNaturalna :: a -> Naturalna

instance Naturalable Int where
    toNaturalna l
        | l <= 0 = Zero -- Nie wiem jak funkcja powinna się zachować dla wartości < 0
        | otherwise = (Kolejna . toNaturalna $ l - 1)

--------------------------------------

-- [10%] ZADANIE 3.3
-- Zdefiniuj funkcję dodającą dwie liczby Naturalne (dodawanie powinno być
-- wykonywane na typie Naturalna! Nie dopuszczalne jest rozwiązanie, w którym
-- dokonujemy konwersji na Int i wykorzystujemy dodawanie na Intach)
-- Przykład:
-- ghci> dodaj (Kolejna (Kolejna Zero)) (Kolejna (Kolejna (Kolejna Zero)))
-- Kolejna (Kolejna (Kolejna (Kolejna (Kolejna Zero))))
-- -- Zakładając, że wcześniejsze konwersje też działają:
-- ghci> naturalnaToInt (dodaj (toNaturalna (3::Int)) (toNaturalna (5::Int)))
-- 8
dodaj :: Naturalna -> Naturalna -> Naturalna
-- dodaj = undefined

dodaj a Zero = a
dodaj Zero b = b
dodaj (Kolejna x) b = dodaj x (Kolejna b)

-----------------------------------------------------------------------------

-- SEKCJA 4: FOLD

-- [20%] ZADANIE 4
-- Napisz funkcję mojFiltr korzystajac z foldr/foldl.
-- Funkcja powinna mieć taką samą funkcjonalność jak wbudowana funkcja filter
mojFiltr :: (a -> Bool) -> [a] -> [a]
-- mojFiltr = undefined

-- To chyba trochę głupie
mojFiltr f list = foldr (\el acc -> if f el then el : acc else acc) [] list

-----------------------------------------------------------------------------

-- SEKCJA 5: TESTY

-- [5*5%] Zdefiniuj testy jednostkowe do wybranych 5 zadań z kolokwium.
-- Dla każdego zadania powinny być dwa testy. Jeden z nich może być przykładem
-- zaczerpniętym z opisu zadania, drugi test wymyśl samodzielnie.
-- Zdefiniuj również funkcję uruchomTesty, która uruchamia zestaw wszystkich
-- przygotowanych przez ciebie testów.

uruchomTesty = undefined
