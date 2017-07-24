--my own list implementation

module Lst where

data IntList = Empty 
               | Cons Int IntList
               deriving Show

--to test
d = (Cons 3 (Cons 1 (Cons 5 (Cons 4 (Cons 2 Empty)))))


head' :: IntList -> Int
head' Empty       = error "Empty list"
head' (Cons x xs) = x 


tail' :: IntList -> IntList 
tail' Empty       = error "Empty list"
tail' (Cons x xs) = xs 

last' :: IntList -> Int
last' Empty          = error "Empty List"
last' (Cons x Empty) = x
last' (Cons x xs)    = last' xs

init' :: IntList -> IntList
init' Empty       = error "Empty List"
init' (Cons x xs) = xs

sum' :: IntList -> Int
sum' Empty       =  0
sum' (Cons x xs) = x + sum' xs


product' :: IntList -> Int
product' Empty       = 1
product' (Cons x xs) = x * product' xs

elem' :: Int  -> IntList -> Bool
elem' _ Empty = False
elem' a (Cons x xs) 
   | x == a     = True
   | otherwise  = elem' a xs


map' :: (Int->Int)  -> IntList -> IntList 
map' _ Empty = error "Empty list"
map' f (Cons x Empty) = (Cons (f x) Empty)
map' f (Cons x xs) = (Cons (f x) (map' f xs))

filter' :: (Int->Bool) -> IntList -> IntList
filter' _ Empty = Empty
filter' f (Cons x xs) 
   | f x       = (Cons x (filter' f xs))
   | otherwise = (filter' f xs)


conc :: IntList -> IntList -> IntList
conc Empty xs          = xs
conc (Cons x Empty) ys = (Cons x ys)
conc (Cons x xs) ys    = (Cons x (conc xs ys))


--there exit some bugs
qSort :: IntList -> IntList 
qSort Empty = error "Empty List"
qSort (Cons x Empty) = (Cons x Empty)
qSort (Cons x xs)    = (conc (conc smaller (Cons x Empty)) larger)
                        where smaller = filter' (<=x) xs
                              larger  = filter' (>x) xs

length' :: IntList -> Int
length' Empty = 0
length' (Cons x xs) = 1 + length' xs

any' :: (Int->Bool) -> IntList -> Bool
any' _ Empty = False
any' f (Cons x xs) 
   | f x       = True
   | otherwise = any' f xs



all' :: (Int->Bool) -> IntList -> Bool
all' _ Empty =  True
all' f (Cons x xs) 
   | (f x)       = all' f xs
   | otherwise   = False




