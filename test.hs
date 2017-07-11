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


sum' :: Num a => [a] -> a
sum'[] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String 
capital "" = "empty string" 
capital all@(x:xs) = "The first letter of "++ all ++ " is " ++ [x]














