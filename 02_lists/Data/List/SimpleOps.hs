-- author: Leander Schulz
-- ----------------------------------------
--
-- simple operations on lists
--
-- run tests:
-- ghci ../../Tests/SimpleListOps.hs
-- > quickCheck prop_nub
--

module Data.List.SimpleOps
where

import Prelude hiding (splitAt)

-- ----------------------------------------

-- | The nub function removes duplicate elements from a list.
--
-- In particular, it keeps only the first occurrence of each element.
-- (The name nub means `essence'.)
--
-- Complexity class: O(n^2)

-- .1 nub with filter

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)


-- .2 nub with list comprehension

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x : xs) = x : [n | n <- nub' xs, n /= x]

-- .3 nub with foldr
-- after chapter about folds

nub'' :: Eq a => [a] -> [a]
nub'' = foldr myfilter []
                where myfilter x xs = (x : filter (/= x) xs) 
-- shorter
nub''' :: Eq a => [a] -> [a]
nub''' = foldr (\ x xs -> x:filter (/= x) xs) []
--              -> implizite Funktionsdeklaration!

-- ----------------------------------------

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- (@splitAt _|_ xs = _|_@).
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.

-- the spec
splitAt :: Int -> [a] -> ([a],[a])
splitAt i xs = (take i xs, drop i xs)

-- the impl
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' _ [] = ([],[])
splitAt' i (x:xs) 
    | i <  1 = ([],(x:xs))
    | i == 1 = ([x],xs)
    | i >  1 = splitHelp i ([x],xs)
    where 
        splitHelp :: Int -> ([a],[a]) -> ([a],[a])
        splitHelp i ([],[]) = ([],[])
        splitHelp i ((x:xs),[]) = ((x:xs),[])
        splitHelp i ((x:xs),(y:ys))
            | i == 1 = ((x:xs),(y:ys))
            | i >  1 = splitHelp (i-1) ([x] ++ xs ++ [y],(ys))
            | otherwise = error "i is not in allowed range"

-- ----------------------------------------

-- | 'intercalate' inserts the list @xs@ in between
-- the lists in @xss@ and concatenates the
-- result.
-- e.g. [1,2] [[2,3,4],[5,6,7]] -> [[2,3,4],[1,2],[5,6,7]]
-- 1. impl: direct or with map
intercalate :: [a] -> [[a]] -> [a]
intercalate _ []     = []
intercalate l (x:[]) = x
intercalate l (x:xs) = x ++ l ++ intercalate l xs

-- 2. impl: with foldr
-- after chapter about folds
intercalate'' :: [a] -> [[a]] -> [a]
intercalate'' l = foldr myfilter []
                  where 
                    -- myfilter [] [] = l
                    myfilter [] x = x
                    myfilter x [] = x
                    myfilter x xs = x ++ l ++ xs     

intercalate' :: [a] -> [[a]] -> [a]
intercalate' l [] = []
intercalate' l (x:xs) = x ++ foldr (\ y ys -> l ++ y ++ ys) [] xs
                --where op y r = l ++ y ++ r 
               
-- ----------------------------------------

-- | The 'partition' function takes a predicate and a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--

-- the spec
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs
  = (filter p xs, filter (not . p) xs)

-- 1. impl: direct
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f [] = ([],[])
partition' f (x:xs) = 
    if f x 
        then ((x:fst next), snd next)
        else (fst next, (x:snd next))           
    where
        next = partition' f xs

-- 2. impl: with foldr
-- after chapter about folds

partition'' :: (a -> Bool) -> [a] -> ([a], [a])
partition'' f = foldr op ([],[])
                    where op x (xs,ys) = if f x then (x:xs,ys) else (xs,x:ys)   

-- ----------------------------------------
--
-- | all prefixes of a list

-- 1. impl: direct

inits        :: [a] -> [[a]]
inits = undefined

-- 2. impl: with foldr
-- after chapter about folds

inits'        :: [a] -> [[a]]
inits' = undefined

-- ----------------------------------------

-- | concatenates 2 lists of strings
-- with a given char in between the elements
--
-- the following law must hold for split and join
--
--   join' c (split' c xs) == xs
--
--   join' c . split c == id
--

join' :: a -> [[a]] -> [a]
join' = undefined

-- | splits the input into sublists at delimiter
--   1. arg is the delimiter
--   the delimiter does not occur in elements of result list

split' :: Eq a => a -> [a] -> [[a]]
split' = undefined

-- ----------------------------------------
