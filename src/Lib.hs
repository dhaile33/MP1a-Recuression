

--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake n [] = []
mytake n (x:xs)
  | n < 0 = []
  | otherwise = x : (mytake (n-1) xs)


--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop 0 xx = xx
mydrop n [] = []
mydrop n (x:xs)
  | n < 0 = (x:xs)
  | otherwise = (mydrop (n-1) xs)

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) P.++ [x]

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app xx yy = xx P.++ yy

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys
myzip _ _ = []
--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs (x:xs) (y:ys) = fst(head (myzip (x:xs) (y:ys))) + snd(head (myzip (x:xs) (y:ys))) : addpairs xs ys
addpairs _ _ = []

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : (addpairs fib (tail fib))

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys) 
  | x < y = x:y:ys
  | x == y = y:ys
  | otherwise = y:(add x ys)

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union [] ys = ys
union xs [] = xs
union (x:xs) (y:ys)
  | x == y = x : (union xs ys)
  | x < y = x : (union xs (y:ys))
  | x > y = y : (union (x:xs) ys)

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys)
  | x == y = x : (intersect xs ys)
  | x < y = (intersect xs (y:ys))
  | otherwise = intersect (x:xs) ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union xss (fmap (x:) xss)
                  where xss = powerset xs

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: (Num a) => [a] -> [a]
inclist' xs = fmap (+1) xs

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' xs = P.foldr (+) 0 xs
