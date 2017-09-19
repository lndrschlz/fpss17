module TruthTable
where

import           Data.List (intercalate)


type TruthTable = [[Bool]]

-- | generate a truth table for n variables

truthTable :: Int -> TruthTable
truthTable n 
  | n == 0 = []
  | n == 1 = [[False],[True]]
  | n >  1 = [e1 ++ [e2] | e1 <- truthTable (n-1), e2 <- [False,True] ]
  | otherwise = error "Unable to build truth table"

ppTruthTable :: TruthTable -> String
ppTruthTable tt
  = unlines (map ppRow tt)
  where
    ppRow r = intercalate " " (map (take 1 . show) r)

printTT :: TruthTable -> IO ()
printTT = putStrLn . ppTruthTable

-- ----------------------------------------
