module Pam where

import Data.Function
sumSquares x y = x ^ 2 + y ^ 2
main = putStrLn "Hello World!"

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n == (-1) = 1
            | n > 1 = fibonacci(n - 1) + fibonacci(n - 2)
            | n < (-1) = fibonacci(n + 2) - fibonacci(n + 1)


fibonacci' :: Integer -> Integer
fibonacci' n | n >= 0 = iter n 1 0
             | n < 0 = iter n 1 0

iter :: Integer -> Integer -> Integer -> Integer
iter n a b | n == 0 = b 
             | n > 0 = iter (n - 1) (a + b) a
             | n < 0 = iter (n + 1) (b - a) a


seqA :: Integer -> Integer

seqA n = let
    iter a b c n | n == 2 = c
    		 | n == 1 = b
     		 | n == 0 = a
                 | otherwise = iter b c (c + b - 2 * a) (n - 1)
    in iter 1 2 3 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = let 
    iter x sumAcc countAcc | x < 10 = (sumAcc + x, countAcc)
      | otherwise = iter (quot x 10) (sumAcc + (rem x 10)) (countAcc + 1)
    in iter (abs x) 0 1

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let
    step = (b - a) / steps 
    steps = 1000
    iter f a b acc n| n == 0 = acc * step / 2 
                    | otherwise = iter f b (b + step) ((f a + f b)  + acc) (n - 1)
    in iter f a (a + step) 0 steps

getSecondFrom :: a -> b -> c -> b
getSecondFrom first second third = second

multSecond = g `on` h

g = (*)

h = snd


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)
