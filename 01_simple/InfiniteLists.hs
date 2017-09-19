-- author: Leander Schulz
module InfiniteLists where

-- | construct a name generator
--
-- names = ["a".."z", "a1".."z1", "a2".."z2", ...]

names :: [String]
names = [[letter] | letter <- ['a'..'z']] ++ [[name] ++ show n | n <- [1..], name <- ['a'..'z'] ]


-- | constructs the infinite sequence
-- of fibonacci numbers in linear time
--
-- fibs = [0, 1, 1, 2, 3, 5, 8, ...]

fibs :: [Integer]
fibs = [0,1] ++ zipWith (+) (tail fibs) fibs

-- ----------------------------------------
--
-- | naive prime number generator with
-- sieve of Eratosthenes and a recursive
-- sieve operation

primes :: [Integer]
primes = phelp [2..]

phelp :: [Integer] -> [Integer]
phelp (x:xs) = [x] ++ phelp (sieve (x:xs))

sieve (x:xs) = [n | n <- xs, n `mod` x /= 0]


-- ----------------------------------------
--
-- | the hamiltonian sequence is the ordered sequence
-- of natural number which are multiples of 2 or 3 or 5
--
-- Implementation: the 3 lists of multiples of 2, 3 and 5
-- are merged together with a @merges@ function.
--
-- The direct solution is

hamilton' :: [Integer]
hamilton'
  = filter
    (\ i ->    i `mod` 2 == 0
            || i `mod` 3 == 0
            || i `mod` 5 == 0
    ) [0..]

-- | @hamilton@ by merging sequences

hamilton :: [Integer]
hamilton
  = merges [is2, is3, is5]
    where
      is2 = [ n | n <- [2..], n `mod` 2 == 0]
      is3 = [ n | n <- [3..], n `mod` 3 == 0]
      is5 = [ n | n <- [5..], n `mod` 5 == 0]

merge :: [Integer] -> [Integer] -> [Integer]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) 
  | x == y = [x] ++ merge xs ys
  | x <  y = [x] ++ merge xs (y:ys)
  | x >  y = [y] ++ merge (x:xs) ys

-- | @merges@ takes a list of lists of ascending integers
-- and merges these lists into a single sorted list without any duplicates
-- direct impl

merges :: [[Integer]] -> [Integer]
merges [] = []
merges (x:xs) = merge x (merges xs)

-- | @merges@ with a fold

merges' :: [[Integer]] -> [Integer]
merges' = undefined    -- after chapter about folds

-- ----------------------------------------
