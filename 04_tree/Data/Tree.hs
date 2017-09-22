-- author: leander schulz

{-# LANGUAGE DeriveDataTypeable #-}

-- ----------------------------------------

-- | binary tree with values at the leafs (Tip),
-- the branches (Bin) don't contain any further information,
-- the empty tree is represented by a special value Null

module Data.Tree
where

import           Prelude             hiding (foldl, foldr, head, tail, init, last)

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Foldable       hiding (toList)
import           Data.Monoid

-- ----------------------------------------

data Tree a
    = Null
    | Tip a
    | Bin (Tree a) (Tree a)
      deriving (Show, Data, Typeable)

-- test values
t1,t2 :: Tree Int
t1 = Bin (Tip 4) (Tip 6)
t2 = Bin (Bin (Tip 1) (Tip 3)) (Bin (Tip 5) (Tip 7))



-- | data type invariant

invTree :: Tree a -> Bool
invTree Null         = True
invTree (Tip x)      = True
invTree (Bin Null _) = False
invTree (Bin _ Null) = False
invTree (Bin l r)    = invTree l && invTree r

-- | smart constructor
bin :: Tree a -> Tree a -> Tree a
bin Null a = a
bin a Null = a
bin a b = Bin a b


instance Functor Tree where
  fmap = undefined

instance Applicative Tree where
  pure  = undefined
  (<*>) = undefined

instance Monad Tree where
  return     = undefined
  _    >>= _ = undefined

instance Alternative Tree where
  empty = mzero   -- or Null
  (<|>) = mplus

instance MonadPlus Tree where
  mzero = undefined
  mplus = undefined

instance Monoid (Tree a) where
  mempty  = undefined
  mappend = undefined

-- fold elements like in a list from right to left
instance Foldable Tree where
--  foldr _ e t = undefined   
  foldr f e Null = e
  foldr f e (Tip x) = f x e
  foldr f e (Bin l r) = foldr f (foldr f e r) l

-- ----------------------------------------
-- classical visitor

visitTree :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
visitTree e tf bf = visit'
  where
    visit' Null = e
    visit' (Tip a) = tf a
    visit' (Bin l r) = bf (visit' l) (visit' r)

-- special visitors

sizeTree :: Tree a -> Int
sizeTree = visitTree 0 (const 1) (+)

minDepth, maxDepth :: Tree a -> Int
minDepth = visitTree 0 (const 0) (\ a b -> (min a b) +1 )
maxDepth = visitTree 0 (const 0) (\ a b -> (max a b) +1 )

-- ----------------------------------------
-- access functions

viewL :: Tree a -> Maybe (a, Tree a)
viewL Null         = Nothing 
viewL (Tip v)      = Just (v,Null)
viewL (Bin Null r) = Just (head r, tail r)
viewL (Bin l r)    = Just (head l, bin (tail l) r) 
    

viewR :: Tree a -> Maybe (Tree a, a)
viewR Null         = Nothing 
viewR (Tip v)      = Just (Null,v)
viewR (Bin l Null) = Just (init l, last l)
viewR (Bin l r)    = Just (bin (init r) l,last r) 

head :: Tree a -> a
head = maybe (error "head: empty tree") fst . viewL

tail :: Tree a -> Tree a
tail = maybe (error "tail: empty tree") snd . viewL

last :: Tree a -> a
last = maybe (error "last: empty tree") snd . viewR

init :: Tree a -> Tree a
init = maybe (error "init: empty tree") fst . viewR

-- ----------------------------------------
-- conversions to/from lists

-- | runs in O(n) due to the use of (:)
toList :: Tree a -> [a]
toList = foldr (:) []

-- | runs in O(n * log n) due to the use of (++)
toListSlow :: Tree a -> [a]
toListSlow = visitTree [] (\ a -> [a]) (++)
            
-- | build a balanced tree
--
-- doesn't work for infinite lists

-- weak balancing criterion
fromList :: [a] -> Tree a
--fromList = fromList'
fromList [] = Null
fromList xs = toTree (map Tip xs)

toTree :: [Tree a] -> Tree a
toTree [] = Null
toTree (x:[]) = x
toTree (x:xs) = toTree (toBin (x:xs))

toBin :: [Tree a] -> [Tree a]
toBin []        = []
toBin (x:[])    = [x]
toBin (x:y:ys)  = [Bin x y] ++ toBin(ys)  

-- creative implementation of building a tree
-- (pretty unbalanced!)
fromList'''' :: [a] -> Tree a
fromList'''' [] = Null
fromList'''' xs = toTreeL (map Tip xs)

toTreeL :: [Tree a] -> Tree a
toTreeL [] = Null
toTreeL (x:[]) = x
toTreeL (x:xs) = Bin x (toTreeR xs)

toTreeR :: [Tree a] -> Tree a
toTreeR [] = Null
toTreeR (x:[]) = x
toTreeR (x:xs) = Bin (toTreeL xs) x


-- strong balancing criterion
fromList' :: [a] -> Tree a
fromList' []     = Null
fromList' (x:[]) = Tip x
fromList' xs = bin (fromList' l) (fromList' r)
            where (l,r) = splitAt (div (length xs) 2) xs

-- list to the right
fromList'' :: [a] -> Tree a
fromList'' = foldr (\ x t -> Tip x `bin` t) Null

-- list to the left
fromList''' :: [a] -> Tree a
fromList''' = foldl (\ t x -> t `bin` Tip x) Null

-- runtime differences between fromList, fromList', fromList'', fromList'''?
-- differences in balancing quality?

-- ----------------------------------------
