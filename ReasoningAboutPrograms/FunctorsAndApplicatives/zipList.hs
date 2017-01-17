--
-- Author: Mirko Bez
-- 
-- Exercise 1
-- 

import Control.Applicative hiding (ZipList)
--help gs xs = [g x | (g,x) <- zip gs xs]

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where 
	--fmap :: (a -> b) -> ZipList a -> ZipList b
	fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
	--pure :: a -> ZipList a
	pure x = Z (repeat x)
	-- <*> :: ZipList (a->b) -> ZipList a -> ZipList b
	(Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

--
-- Exercise 2
--

--
--
--
exA l1 l2 l3 = [x:y:z:[] | x <- l1, y <- l2, z <- l3]

---
---
---
exB :: [Int] -> [Int] -> [Int] -> [[Int]]
exB l1 l2 l3 = toList(pure (\x -> \y -> \z -> x:y:z:[]) <*> Z l1 <*> Z l2 <*> Z l3)

--
--
--
exC :: [[Int]] -> [[Int]]
exC l = toList(pure (\g -> \y -> map g y) <*>  Z (map (+) [1..]) <*> Z l)

-- Function that extract the list from the dummy container
toList :: ZipList a -> [a]
toList (Z xs) = xs




