{-# LANGUAGE FlexibleInstances #-}
import Data.Maybe

-- Exercise 2
data Osoba = Osoba
    { name      :: String
    , surname   :: String
    , pesel     :: String
    }

class EqualPesel a where
    ep :: a -> a -> Bool

class GreaterSurname a where
    gs :: a -> a -> Bool
   
instance EqualPesel Osoba where
    ep x y = (pesel x) == (pesel y)

instance GreaterSurname Osoba where
    gs x y = (surname x) > (surname y)

-- Exercise 3
createMonad :: [Osoba] -> Maybe Osoba
createMonad [] = Nothing
createMonad list = Just (head list)

find :: [Osoba] -> Osoba -> Maybe Osoba
find list el = createMonad (filter (\a -> a `ep` el) list)

-- Test data
szymon = Osoba "Szymon" "Bobek" "12345678901"
bobek = Osoba "S" "Bobek"  "12345678901"
zenon = Osoba "Zenon" "Adamczyk" "111111111"
adamczyk = Osoba "Zenon" "Adamczyk" "222222222"
peopleList = [szymon, zenon]

--Exercise 4
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

insert :: Ord a => Tree a -> a -> Tree a
insert Empty el = Node el Empty Empty
insert (Node x lt rt) el
    | x == el = Node x lt rt
    | el < x = Node x (insert lt el) rt
    | otherwise = Node x lt (insert rt el)

empty :: Tree a -> Bool
empty Empty = True
empty (Node x lt rt) = False

search :: Ord a => Tree a -> a -> Bool
search Empty el = False
search (Node x lt rt) el
    | x == el = True
    | el < x = search lt el
    | otherwise = search rt el

toString :: Show a => Tree a -> String
toString Empty = ""
toString (Node x lt rt) = show x ++ "(" ++ (toString lt) ++ "," ++ (toString rt) ++ ")"

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Node x lt rt) = [x] ++ leaves lt ++ leaves rt

nnodes :: Tree a -> Int
nnodes Empty = 0
nnodes (Node x lt rt) = 1 + (nnodes lt) + (nnodes rt)

nsum :: Num a => Tree a -> a
nsum Empty = 0
nsum (Node x lt rt) = x + (nsum lt) + (nsum rt)

remove :: (Ord a) => Tree a -> a -> Tree a
remove Empty _ = Empty
remove (Node x lt rt) el
    | el < x = Node x (remove lt el) rt
    | el > x = Node x lt (remove rt el)
    | lt == Empty = rt
    | rt == Empty = lt
    | otherwise = Node (maxElement lt) (remove lt (maxElement lt)) rt

-- Used by 'remove' function
maxElement :: (Ord a) => Tree a -> a
maxElement (Node x lt rt)
    | rt /= Empty = maxElement rt
    | otherwise = x

-- Test data
myTree :: Tree Int
myTree = Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)
myTree2 = Empty