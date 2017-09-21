-- author = leander schulz
module Data.IntervalSet
where

-- ----------------------------------------

-- a pair of Ints can be represent with closed intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2) = x1 `max` x2 <= 1 + (y1 `min` y2)

less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2)
  = y1 < x2


nullInterval :: Interval -> Bool
nullInterval (x, y) = x > y 


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge (x1, y1) (x2, y2) = (min x1 x2, max y1 y2)


-- ----------------------------------------
-- http://www.zeit.de/campus/2017/s2/informatik-mathematik-master-vorteile-studium-karriere
-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]
is1, is2, is3 :: IntervalSet
is1 = [(1,2),(4,5),(7,8)]
is2 = [(11,12),(14,15),(116,118)]
is3 = [(2,6),(10,11)]

inv :: IntervalSet -> Bool
inv [] = True
inv (x:xs)  
    | nullInterval x      = False
    | xs == []            = True
    | overlap x (head xs) = False
    | less x (head xs)    = inv xs
    | otherwise           = False

-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval i [] = [i]
insertInterval i (is:iss) 
    | nullInterval i        = (is:iss)
    | overlap i is          = (merge i is) : iss
    | less i is             = [i] ++ (is:iss) 
    | otherwise             = is : insertInterval i iss

fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList [] = []
fromIntervalList (i:[]) = [i]
fromIntervalList (i:is)
    | nullInterval i = fromIntervalList is
    | otherwise = union [i] (fromIntervalList is)


-- ----------------------------------------
--
-- exported ops, names similar to Data.Set

empty :: IntervalSet
empty = []

singleton :: Int -> IntervalSet
singleton i = singleInterval i i

insert :: Int -> IntervalSet -> IntervalSet
insert i = insertInterval (i, i)

union :: IntervalSet -> IntervalSet -> IntervalSet
union = foldr insertInterval
--union [] (b:bs) = (b:bs)
--union (a:as) [] = (a:as)
--union (a:as) (b:bs)
--    | overlap a b          = (merge a b) : union as bs
--    | less a b             = union as ([a] ++ (b:bs)) 
--    | otherwise            = union (a:(b:as)) bs

member :: Int -> IntervalSet -> Bool
member i [] = False
member i (is:iss) = overlap (i,i) is || member i iss


fromList :: [Int] -> IntervalSet
fromList = foldr insert empty


toList :: IntervalSet -> [Int]
toList [] = []
toList (i:xs) = [(fst i) .. (snd i)] ++ toList xs


-- ----------------------------------------
