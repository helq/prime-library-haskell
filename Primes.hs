module Primes (
   primes
 , isPrime
 , factorPrimes
 , intsInFactors
 ) where

-- by helq

import Data.List (find)

--primes :: Integral a => [a]
--primes = [2,3,5,7,11,13,17] ++ [x|x<-[19,21..], isPrime x]

primes :: Integral a => [a]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
         ++ [x|x<-posible, isPrime' x]
    where posible = f 101 $ cycle [2,4,2,4,6,2,6,4] -- no divisible for 2, 3 or 5
            where f a (x:xs) = a : f (a+x) xs

          isPrime' :: Integral a => a -> Bool
          isPrime' n = not . any (divide n)
                     . takeLeastSqrt n $ drop 3 primes


isPrime :: Integral a => a -> Bool
isPrime n | n < 2     = False
          | otherwise = not . any (divide n) . takeLeastSqrt n $ primes

divide :: Integral a => a -> a -> Bool
x `divide` p = x `rem` p == 0

sqrtIntegral :: Integral a => a -> a
sqrtIntegral = truncate . sqrt . fromIntegral

takeLeastSqrt :: Integral a => a -> [a] -> [a]
takeLeastSqrt n = takeWhile (<= sqrtIntegral n)

factorPrimes :: Integral a => a -> [a]
factorPrimes n
    | n < 2     = []
    | otherwise = factor : factorPrimes rest
    where factor = case find (divide n) $ takeLeastSqrt n primes of
                      Just p  -> p -- p is factor of n
                      Nothing -> n -- then n is prime
          rest = n `div` factor

intsInFactors :: Integral a => [(a,[a])]
intsInFactors = zip [2..] $ list primes [2..]
    where list (p:ps) (n:ns)
            | n < p     = factorPrimes n : list (p:ps) ns
            | otherwise = [p] : list ps ns
