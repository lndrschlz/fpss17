module Data.FunctionalList
where

import           Prelude (Bool(..), (.), (++), undefined)
import qualified Prelude as P

type List a = [a] -> [a]

-------------------------------------------
fromList        :: [a] -> List a
fromList l      = \ x -> l ++ x  

toList          :: List a -> [a]
toList l        = l []

empty           :: List a
empty           = fromList []

singleton       :: a -> List a
singleton e     = fromList [e]

-- (:) for functional lists
-- test case: toList P.$ cons 1 (fromList [2,3,4,5])
cons            :: a -> List a -> List a
cons e l        = \ x -> [e] ++ l x

-- dual to cons
-- test case: toList P.$ snoc (fromList [1,2,3,4,5]) 6
snoc            :: List a -> a -> List a
snoc l e        = \ x -> l ([e] ++ x)

-- (++) for functional lists
-- test: toList P.$ append (fromList [1,2,3]) (fromList [4,5,6])
append          :: List a -> List a -> List a
append l1 l2    = l1 . l2

-- like concat for normal lists: foldr (++) []
-- test: toList P.$ concat [(fromList [1,2,3]),(fromList [4,5,6])]
concat          :: [List a] -> List a
concat          = P.foldr (append) empty

-- like map for normal lists: foldr ((:) . f) []
-- test: toList P.$ map (\ x -> P.show x) (fromList [1,2,3,4])
map             :: (a -> b) -> List a -> List b
map f l         = P.foldr (cons . f) empty P.$ l []

-- foldr with foldr for normal lists
-- test: foldr (P.+) 0 P.$ fromList [1,2,3,4,5]
foldr           :: (a -> b -> b) -> b -> List a -> b
foldr op n      = P.foldr op n . toList

-- head, tail, null
head            :: List a -> a
head            = P.head . toList

tail            :: List a -> List a
tail l          = P.tail . l

null            :: List a -> Bool
null            = P.null . toList

reverse         :: List a -> List a
reverse l       = P.reverse . l

-- ----------------------------------------
