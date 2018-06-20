-- Otwarte:
-- 1. Zrobić typa danych Student z jakimiś tam polami

data Student = Student
    {
        name    :: String,
        age     :: Int
    }

-- 2. Do tej klasy co wyżej dopisać "instance" dla Show i Eq

instance Eq Student where
    a == b = (name a) == (name b) && (age a) == (age b)
    a /= b = (name a) /= (name b) || (age a) /= (age b)

instance Show Student where
    show s = (name s) ++ " is currently " ++ (show $ age s) ++ " years old."

-- Test data

szymon = Student "Szymon" 33
bobek = Student "Szymon" 33
zenon = Student "Zenon" 19

-- Zakmnięte:
-- 1. Jak Haskell dopasowuje funkcje (chyba coś takiego)

-- Po kolei jak są zadeklarowane? Nie wiem, chyba chodzi o pattern matching czy coś.

-- 2. Co zrobi (-) 2 4

minus = (-) 2 4 -- Zwraca (-2)

-- 3. Cos z "return" i wypisywaniem dwóch stringów
-- 4. Co zrobi take 20 ([1..])

take20 = take 20 [1..] -- Zwróci listę zawierającą int od 1 do 20 włącznie

-- 5. Funkcja fun a = a*a, jaki będzie nagłówek funkcji

fun :: Num a => a -> a
fun a = a*a -- W nagłówku i deklaracji jest a, ale lepiej na to zwracać uwagę bo czasem haskell miewa problemy

-- 6. Mamy data X a = M a | P a, co się stanie, gdy M "a" == P "a"

data X a = M a | P a deriving (Eq) -- deriving ja dodałem, bez tego wyrzuci błąd, więc nie wiem czy to nie poprawna odpowiedź

testXdata a = M a == P a -- Wywołanie \"testXdata "a"\" zwraca wartość False (dla innych stringów tak samo)


-- Opis reszty pytań raczej mało precyzyjny, więc wiele nie zrobiłem

-- Reszta jak pamiętam to wskazać co jest niepoprawne albo jakieś fałszywe stwierdzenia,
-- najczęściej nawias w złym miejscu lub argumenty w złej kolejności
-- Skladanie funkcji . i w poloczeniu ze spacja i nawiasami, ktoro bedzie dzialac

-- Czy funkcje w haskellu sie zawsze zakoncza?
-- Cholera wie. Ale po namyśle wydaje mi się, ze tak. Myląca jest funkcja main.
-- Ale z drugiej strony przecie mozna napisać nieskończoną rekurencje.

-- Czy istnieje wielowatkowosc?
-- Wszysto co widzę wskazuje na to, ze jest

-- Przyklad gdzie nie ma Eq i co sie stanie gdy chcemy zrobic show
-- Nie wiem co ma Eq do funkcji show