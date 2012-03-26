-- | Tests for the 'Tournament' module.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified Game.Tournament as T
import Test.QuickCheck
import Data.List ((\\), nub, genericLength)
import Control.Monad (liftM)

-- helper instances for positive short ints
newtype RInt = RInt {rInt :: Int} deriving (Eq, Ord, Show, Num, Integral, Real, Enum)
newtype SInt = SInt {sInt :: Int} deriving (Eq, Ord, Show, Num, Integral, Real, Enum)
instance Arbitrary RInt where arbitrary = liftM RInt (choose (1, 256) :: Gen Int)
instance Arbitrary SInt where arbitrary = liftM SInt (choose (1, 16) :: Gen Int)

-- -----------------------------------------------------------------------------
-- inGroupsOf
-- test positive n <= 256, s <= 16
type GroupArgs = (RInt, SInt)

-- group sizes <= input size
groupsProp1 :: GroupArgs -> Bool
groupsProp1 (n', s') = maximum (map length (n `T.inGroupsOf` s)) <= s where
  (n, s) = (fromIntegral n', fromIntegral s')

-- players included == [1..n]
groupsProp2 :: GroupArgs -> Bool
groupsProp2 (n', s') = length pls == n && null (pls \\ [1..n]) where
  pls = concat $ n `T.inGroupsOf` s
  (n, s) = (fromIntegral n', fromIntegral s')

-- sum of seeds of groups in full groups differ by at most num_groups
groupsProp3 :: GroupArgs -> Property
groupsProp3 (n', s') = n `mod` s == 0 ==>
  maximum gsums <= minimum gsums + length gs where
    gs = n `T.inGroupsOf` s
    gsums = map sum gs
    (n, s) = (fromIntegral n', fromIntegral s')

-- sum of seeds is perfect when groups are full and even sized
groupsProp4 :: GroupArgs -> Property
groupsProp4 (n', s') = n `mod` s == 0 && even (n `div` s) ==>
  maximum gsums == minimum gsums where
    gsums = map sum $ n `T.inGroupsOf` s
    (n, s) = (fromIntegral n', fromIntegral s')

-- -----------------------------------------------------------------------------
-- robin
-- test positive n <= 256

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
  pairsFor k = concatMap (filter (\(x,y) -> x == k || y == k)) $ T.robin n
  combatants k = map (\(x,y) -> if x == k then y else x) $ pairsFor k

-- -----------------------------------------------------------------------------
-- eliminationOf
-- test positive n <= 256

-- -----------------------------------------------------------------------------
-- Test harness

qc = quickCheckWith stdArgs {
    maxSuccess = 50
  , chatty = True
  }

main :: IO ()
main = do
  putStrLn "test robin:"
  {-mapM_ qc [
      robinProp1
    , robinProp2
    , robinProp3
    , robinProp4
    ]-}
  --putStrLn "test inGroupsOf"
  mapM_ qc [
      groupsProp1
    , groupsProp2
    ]
  -- need a better thing to use as qc
  -- else its type becomes rigid after one run

  putStrLn "done"
