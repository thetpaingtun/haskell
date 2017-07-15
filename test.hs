import Data.List
import System.IO



int = 3


--fact 0 = 1
--fact n = n * fact (n-1)


double x = x + x



quad x = double (double x)


fact n = product[1..n]


avg ns = sum ns `div` length ns

person = ("paing",24)



add :: Int -> (Int->Int)
add x y = x + y

add2 :: Int -> Int -> Int
add2 x y = x + y


add3 :: Num a => a -> a -> a
add3 x y = x + y



lucky :: Integral a => a -> String
lucky 7 = "You are lucky"
lucky x = "Sorry"



charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Bob"
charName x = "Invalid char"


addVectors :: Num a => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)


first :: (a,b,c) -> a
first (a,_,_) = a

second :: (a,b,c)-> b
second (_,b,_) = b

third :: (a,b,c) -> c
third (_,_,c) = c

head' :: [a] -> a
head' [] = error "list is empty"
head' (x:_) = x

tell :: Show a => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element "++ show x
tell (x:y:[]) = "The list has two elements "++ show x ++ " and "++ show y
tell (x:y:_) = "The list is long and the first two elements are "++ show x ++ " and " ++ show y


length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs 


capital :: String -> String 
capital "" = "empty string" 
capital all@(x:xs) = "The first letter of "++ all ++ " is " ++ [x]


bmiTell :: Float -> String
bmiTell bmi
   | bmi <= 18.5 = "underweight"
   | bmi <= 25.0 = "normal"
   | bmi <= 30.0 = "overweight"
   | otherwise = "a whale"



calBmi :: Float -> Float -> String
calBmi w h 
   | bmi <= skinny = "skinny"
   | bmi <= normal = "normal"
   | bmi <= fat    = "fat"
   | otherwise     = "a whale"
   where bmi = w / h^2
         (skinny,normal,fat) = (18.5,25,30)

max' :: Ord a => a -> a -> a
max' a b 
   | a > b = a
   | otherwise = b





mycompare :: Ord a => a -> a -> Ordering
a `mycompare` b 
   | a > b     = GT
   | a == b    = EQ
   | otherwise = LT




initials :: String -> String -> String
initials (x:xs) (y:ys) = [x] ++ "." ++ [y]


calBmis :: [(Float,Float)] -> [Float]
calBmis xs = [bmi w h | (w,h) <- xs]
   where bmi w h = w / h^2



cylinder :: Float -> Float -> Float 
cylinder r h = 
   let sidearea = 2 * 3.14 * r * h
       toparea = 3.14 * r^2
   in  sidearea + 2 * toparea




hhead :: [a] -> a
hhead xs = case xs of [] -> error "Empty list"
                      (x:_) -> x


doubleSmallNumber x = if x > 100
                        then x
                        else x * 2


maxlist :: Ord a => [a] -> a
maxlist [] = error "empty list"
maxlist (x:[]) = x 
maxlist (x:xs)  = max x (maxlist xs)


id' :: a -> a
id' x = x


palindrome :: String -> Bool
palindrome xs = reverse xs == xs

const :: a -> b -> a
const x _ = x

cons :: a -> (b->a)
cons x = \_ -> x


--odds n = map f [0..n-1]
  --       where f x = x * 2 +1


odds n = map (\x -> x * 2 +1) [0..n-1]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

numOfFactors :: Int -> Int
numOfFactors n = length (factors n)

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], isPrime x]


pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs) 

sorted :: Ord a => [a] -> Bool
sorted xs = and [x<=y | (x,y) <- pairs xs]


repl :: Int -> a -> [a]
repl n a = [a | _ <- [1..n]]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n],isPerfect x]
               where isPerfect num = sum (init (factors num)) == num


sum' :: Num a => [a] -> a
sum' [] = error "empty list"
sum' (x:[]) = x
sum' (x:xs) = x + sum' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = (last xs) : (reverse (init xs))


zipTogether :: [a] -> [b] -> [(a,b)]
zipTogether [] _ = []
zipTogether _ [] = []
zipTogether (x:xs) (y:ys) = (x,y) : (zipTogether xs ys)


nextChar :: Char -> Char
nextChar c 
   | c == 'z' = 'a'
   | otherwise = succ c



rotateChar :: Char -> Int -> Char
rotateChar c 0 = c
rotateChar c n = rotateChar (nextChar c) (n-1)


cipher :: String -> Int -> String
cipher [] _ = []
cipher (x:xs) n = (rotateChar x n) : (cipher xs n) 



prevChar :: Char -> Char 
prevChar c 
   | c == 'a' = 'z'
   | otherwise = pred c


rollChar :: Char -> Int -> Char
rollChar c 0 = c
rollChar c n = rollChar (prevChar c) (n-1)


decipher :: String -> Int -> String
decipher "" _ = []
decipher (x:xs) n = (rollChar x n) : (decipher xs n)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
   | n <=0 = []
   | otherwise = x : (take' (n-1) xs)


rev' :: [a] -> [a]
rev' [] = []
rev' (x:xs) = reverse' xs ++ [x] 

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
   | y == x = True
   | otherwise = elem' y xs 



map' :: (a->b)-> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : (map f xs)

filter' :: (a->Bool)-> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
   | f x       = x : filter' f xs 
   | otherwise = filter' f xs 


foldr' :: Num a => (a->a->a) -> a -> [a] -> a
foldr' _ [] = 0
foldr' f v (x:xs) = f (f x v) (foldr xs) 
