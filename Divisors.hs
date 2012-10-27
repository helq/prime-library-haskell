module Divisors (
      divisors
    , numDivisors
    ) where

import Primes (factorPrimes)
import Data.List (group)

groupFactPrimes :: Integral a => a -> [(a, Int)]
groupFactPrimes n = map (\xs@(x:_)->(x, length xs)) $ group $ factorPrimes n

numDivisors :: (Integral a, Integral b) => a -> b
numDivisors n = f $ map (fromIntegral.snd) $ groupFactPrimes n
    where f = foldr (\n ns->(n+1)*ns) 1

divisors :: Integral a => a -> [a]
divisors = map product . divisorsInFactors

divisorsInFactors :: Integral a => a -> [[a]]
divisorsInFactors n = nPerms' $ groupFactPrimes n
    where 
        --nPerms []     = [[]]
        --nPerms (x:xs) = nFactors x $ nPerms xs

        --nFactors :: (a, Int) -> [[a]] -> [[a]]
        --nFactors (x, n) xs
        --    | n == 0    = xs
        --    | otherwise = xs ++ nFactors (x, n-1) (map (x:) xs)

        nPerms' []     = [[]]
        nPerms' ((x,n):xs) = concatMap (f (x,n)) $ nPerms' xs

        f :: (a, Int) -> [a] -> [[a]]
        --f (x, n) m = foldr (\_ ys->(x:head ys):ys) [m] [1..n]
        f (x, n) m
            | n == 0    = [m]
            | otherwise = m : map (x:) (f (x, n-1) m)
