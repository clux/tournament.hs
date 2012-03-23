-- | Tests for the 'Tournament' module.

module Main where

import qualified Tournament as T
import Test.QuickCheck (quickCheck)
import Data.List ((\\), nub)

-- -----------------------------------------------------------------------------
-- inGroupsOf

-- group size <= input size
groupsProp1 :: Int -> Int -> Property
groupsProp1 n s = n >= 0 && s >= 0 ==>
  let gs = n `T.inGroupsOf` s
  in maximum (map length gs) <= s

-- players included == [1..n]
groupsProp2 :: Int -> Int -> Property
groupsProp2 n s = n >= 0 && s >= 0 ==>
  let pls = concat $ n `T.inGroupsOf` s
  in length pls == n && null $ pls \\ [1..n]

-- sum of seeds of groups in full groups differ by at most num_groups
groupsProp3 :: Int -> Int -> Property
groupsProp3 n s = n >= 0 && s >= 0 && n `mod` s == 0 ==>
  let gs = n `T.inGroupsOf` s
      ms = map sum gs
  in maximum ms <= minimum ms + length gs

-- sum of seeds is perfect when groups are full and even sized
groupsProp4 :: Int -> Int -> Property
groupsProp4 n s = n >= 0 && s >= 0 && n `mod` s == 0 && even (n `div` s) ==>
  let gs = n `T.inGroupsOf` s
      ms = map sum gs
  in maximum ms == minimum ms


--quickCheck (groupsProp3 :: Int -> Int -> Property) ? need cast still? how to import?

-- -----------------------------------------------------------------------------
-- robin

-- correct number of rounds
robinProp1 :: Int -> Property
robinProp1 n = n >= 2 ==>
  let rs = robin n in length rs == (if odd n then n else n-1)

-- each round contains the correct number of matches
robinProp2 :: Int -> Property
robinProp2 n = n >= 2 ==>
  let rs = robin n in all (== n `div` 2) $ map length rs

-- a player is uniquely listed in each round
robinProp3 :: Int -> Property
robinProp3 n = n >= 2 ==>
  let rs = robin n
      plrs = map (concatMap (\(x,y) -> [x,y])) rs
  in map nub plrs == plrs


-- a player is playing all opponents [hence all exactly once by 3]
robinProp4 :: Int -> Property
robinProp4 n = n >= 2 ==>
  let rs = robin n
      pairsFor k = concatMap (filter (\(x,y) -> x == k || y == k)) rs
      combatants k = map (\(x,y) -> if x == k then y else x) $ pairsFor k
  in all (\i -> [1..n] \\ combatants i == [i]) [1..n]


-- -----------------------------------------------------------------------------
-- eliminationOf

-- -----------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = tests
