module Test.FunctionalMap
where

import           Data.FunctionalMap
import           Prelude            hiding (lookup)
import qualified Prelude            as P
import           Test.QuickCheck

prop_insert :: (Int, Int) -> Bool
prop_insert (k,v) = (lookup k (insert k v empty)) == Just v

-- prop_delete :: [(Int, Int)] -> Bool
-- prop_delete (x:xs) =  delete (fst x) (fromList (x:xs))
--     ==   


prop_3 :: String -> Bool
prop_3 xs = undefined

-- ----------------------------------------

quickCheck' :: Testable prop => prop -> IO ()
quickCheck' = quickCheckWith stdArgs{maxSuccess=200}

main
  = do quickCheck'  prop_insert
       -- quickCheck'  prop_2
       -- quickCheck'  prop_3
       
-- ----------------------------------------

