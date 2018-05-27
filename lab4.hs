{-# LANGUAGE FlexibleInstances #-}
import Data.Maybe

data Osoba = Osoba
    { name      :: String
    , surname   :: String
    , pesel     :: String
    } deriving (Show)

class Eqq a where
    (===) :: a -> a -> Bool

class GreaterSurname a where
    gs :: a -> a -> Bool
   
instance Eqq Osoba where
    x === y = ((pesel x) == (pesel y))

instance GreaterSurname Osoba where
    gs x y = (surname x) > (surname y)

szymon = Osoba "Szymon" "Bobek" "12345678901"
bobek = Osoba "S" "Bobek"  "12345678901"
zenon = Osoba "Zenon" "Adamczyk" "111111111"
test = Osoba "Zenon" "Adamczyk" "222222222"

wow = [szymon, bobek]

find :: [Osoba] -> Osoba -> Maybe Osoba

createMonad :: [Osoba] -> Maybe Osoba
createMonad [] = Nothing
createMonad list = Just (head list)

find list el = createMonad (filter (\a -> a === el) list)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

maxt :: Ord a => Tree a -> a

maxt (Node x t1 t2) = max x (max y z)
    where
        y = if (t1 == Empty) then x
            else maxt t1
        z = if (t2 == Empty) then x
            else maxt t2

search :: Ord a => Tree a -> a -> Bool

search Empty el = False
search (Node x lt rt) el
    |x == el = True
    |el < x = search lt el
    |otherwise = search rt el

insert :: Ord a => Tree a -> a -> Tree a

insert Empty el = Node el Empty Empty
insert (Node x lt rt) el
    |x == el = Node x lt rt
    |el < x = Node x (insert lt el) rt
    |otherwise = Node x lt (insert rt el)

empty :: Tree a -> Bool

empty Empty = True
empty (Node x lt rt) = False

myTree :: Tree Int
-- myTree = Node 1 (Node 2 Empty (Node 3 Empty Empty)) (Node 4 Empty Empty)
myTree = Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)
myTree2 = Empty