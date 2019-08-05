module Dice 
    (
      doRoll
    , doRolls
    , doRollsWithMod
    )
where

import System.Random

roll :: Int -> StdGen -> (Int, StdGen)
roll n = randomR (1,n)

rolls :: (Int, Int) -> StdGen -> [Int]
rolls (_, 0) _ = []
rolls (n, x) g = let (i, ng) = roll n g
                 in i : rolls (n, x-1) ng

doRoll :: Int -> IO Int
doRoll n = fst . roll n <$> newStdGen
  
doRolls :: (Int, Int) -> IO [Int]
doRolls (n, x) = rolls (n, x) <$> newStdGen 

doRollsWithMod :: (Int, Int) -> (Int -> Int) -> IO [Int]
doRollsWithMod (n, x) m = do
  rollsR <- doRolls (n, x)
  return $ map m rollsR
