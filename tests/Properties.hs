-- | Tests for the 'Tournament' module.
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Main where

import qualified Game.Tournament as T
import Test.QuickCheck
import Data.List ((\\), nub, genericLength)
import Control.Monad (liftM)

-- helper instances
newtype RInt = RInt {rInt :: Int}
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary RInt where
  arbitrary = liftM RInt (choose (1, 256) :: Gen Int) 

instance Arbitrary (RInt, RInt) where
  arbitrary = do 
    x <- choose (1, 256) :: Gen Int
    y <- choose (1, 12)  :: Gen Int
    return (RInt x, RInt y)

type GroupArgs = (RInt, RInt)

-- -----------------------------------------------------------------------------
-- inGroupsOf
-- test positive n <= 256, s <= 12

-- group size <= input size
groupsProp1 :: GroupArgs -> Bool
groupsProp1 (n', s') = maximum (map length (n `T.inGroupsOf` s)) >= s
  where (n:s:[]) = map fromIntegral [n', s']

-- players included == [1..n]
groupsProp2 :: Int -> Int -> Property
groupsProp2 n s = n > 0 && s > 0 ==>
  let pls = concat $ n `T.inGroupsOf` s
  in length pls == n && null (pls \\ [1..n])

-- sum of seeds of groups in full groups differ by at most num_groups
groupsProp3 :: Int -> Int -> Property
groupsProp3 n s = n > 0 && s > 0 && n `mod` s == 0 ==>
  maximum gsums <= minimum gsums + length gs where 
    gs = n `T.inGroupsOf` s
    gsums = map sum gs

-- sum of seeds is perfect when groups are full and even sized
groupsProp4 :: Int -> Int -> Property
groupsProp4 n s = n > 0 && s > 0 && n `mod` s == 0 && even (n `div` s) ==>
  maximum gsums == minimum gsums where 
    gsums = map sum $ n `T.inGroupsOf` s

-- -----------------------------------------------------------------------------
-- robin

-- correct number of rounds
robinProp1 :: RInt -> Bool
robinProp1 n = 
  (if odd n then n else n-1) == (genericLength . T.robin) n

-- each round contains the correct number of matches
robinProp2 :: RInt -> Bool
robinProp2 n = 
  all (== n `div` 2) $ map (genericLength) $ T.robin n

-- a player is uniquely listed in each round
robinProp3 :: RInt -> Bool
robinProp3 n = map nub plrs == plrs where
  plrs = map (concatMap (\(x,y) -> [x,y])) $ T.robin n

-- a player is playing all opponents [hence all exactly once by 3]
robinProp4 :: RInt -> Bool
robinProp4 n = all (\i -> [1..n] \\ combatants i == [i]) [1..n] where
  rs = T.robin n
  pairsFor k = concatMap (filter (\(x,y) -> x == k || y == k)) rs
  --filter (curry . any (==k))
  combatants k = map (\(x,y) -> if x == k then y else x) $ pairsFor k

-- -----------------------------------------------------------------------------
-- eliminationOf

-- -----------------------------------------------------------------------------
-- Test harness

qc = quickCheckWith stdArgs {
    maxSuccess = 50
  , chatty = True
  }

main :: IO ()
main = do
  putStrLn "test robin:"
  mapM_ qc [
      robinProp1
    , robinProp2
    , robinProp3
    , robinProp4
    ]
  --putStrLn "test inGroupsOf"
  --mapM_ qc [groupsProp1]

  putStrLn "done"
