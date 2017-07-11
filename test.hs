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






