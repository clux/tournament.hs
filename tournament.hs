-----------------------------------------------------------------------------
--
-- Module      :  Tournament
-- Copyright   :  (c) clux 2012
-- License     :  GPL
--
-- Maintainer  :  clux
-- Stability   :  unstable
-- Portability :  unknown
--
-- Tournament related algorithms
--
-----------------------------------------------------------------------------
module Tournament (
   -- * Duel helpers

     seeds             -- :: Int -> Int -> (Int, Int)
   , duelValid         -- :: Int -> (Int, Int) -> Bool

   -- * Group helpers
   , inGroupsOf        -- :: Int -> Int -> [[Int]]
   , robin             -- :: Int -> [[(Int, Int)]]

   -- * Duel eliminationOf
   , eliminationOf     -- :: Elimination -> PlayerCount -> [Match]


   -- * TODO: what to do here?
   , main

) where

import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase, readInt)
import Data.List (sort, splitAt)
import Data.Bits (shiftL)



main = do
  print $ seeds 3 4
  print $ 15 `inGroupsOf` 5
  print $ 15 `inGroupsOf` 3
  print $ 16 `inGroupsOf` 4
  print $ Single `eliminationOf` 8


-- -----------------------------------------------------------------------------
-- Duel Helpers
-- Based on the theory from http://clux.org/entries/view/2407
-- TODO should somehow ensure 0 < i <= 2^(p-1) in the next fn

-- | Computes both the player seeds (in order) for a duel elimiation match.
-- The first argument, p, is the power of the tournament,
-- and the second, i, is the match number.
-- Well-defined for n > 0 and 0 < i <= 2^(p-1)
seeds :: Int -> Int -> (Int, Int)
seeds p i = (1 - last + 2^p, last) where
  last = let (k, r) = ((floor . logBase 2 . fromIntegral) i, i - 2^k) in
    case r of
      0 -> 2^(p-k)
      _ -> let bstr = reverse $ showIntAtBase 2 intToDigit (i - 2*r) ""
               nr = fst $ readInt 2 (`elem` "01") digitToInt bstr !! 0
          in 2^(p-k-1) + nr `shiftL` (p - length bstr)


-- | Check if the 3 criteria for perfect seeding holds for the current
-- power and seed pair arguments.
-- This can be used to make a measure of how good the seeding was in retrospect
duelValid :: Int -> (Int, Int) -> Bool
duelValid n (a, b) = odd a && even b && a + b == 1 + 2^n


-- -----------------------------------------------------------------------------
-- Group helpers

-- | Splits a numer of players into groups of as close to equal seeding sum
-- as possible. When groupsize is even and s | n, the seed sum is constant.
inGroupsOf :: Int -> Int -> [[Int]]
0 `inGroupsOf` _ = []
n `inGroupsOf` s = map (sort . filter (<=n) . makeGroup) [1..ngrps] where
  ngrps = ceiling $ fromIntegral n / fromIntegral s
  s' = s - head (filter (\x -> n > ngrps*(s-1-x)) [0..]) -- reduce s if unfillable
  n' = ngrps*s' -- n if filled groups != n (10 inGroupsOf 4 uses n' = 12)
  nppg = s' `div` 2 -- num pair per group
  np = nppg * ngrps  -- total num pairs
  ps = take np $ zip [1..] [n', n'-1..] -- all the pairs
  rem = [np+1, np+2 .. n'-np] -- e in [1..n'] \\ ps
  makeGroup i = concatMap (\(x,y) -> [x,y]) pairs ++ left where
    pairs = filter ((`elem` [i, i+ngrps .. i+np]) . fst) ps
    left = if length rem >= i then [rem !! (i-1)] else []


-- | Round robin schedules a list of n players and returns
-- a list of rounds (where a round is a list of pairs). Uses
-- http://en.wikipedia.org/wiki/Round-robin_tournament#Scheduling_algorithm
robin :: Int -> [[(Int, Int)]]
robin n = map (filter notDummy . toPairs) rounds where
  n' = if odd n then n+1 else n
  m = n' `div` 2 -- matches per round
  permute (x:xs) = x : (last xs) : (init xs)
  rounds = take (n'-1) $ iterate permute [1..n']
  notDummy (x,y) = all (<=n) [x,y]
  toPairs x =  take m $ zip x (reverse x)

-- -----------------------------------------------------------------------------
-- Duel elimination

type PlayerCount = Int

data Bracket = Losers | Winners deriving (Show, Eq, Ord)


-- Location fully determines the place of a match in a tournament
type Round = Int
type MatchNum = Int

data Location = Location {
  brac :: Bracket
, rnd  :: Round
, num  :: MatchNum
} deriving (Show, Eq)

type Players = [PlayerCount]
type Scores = Maybe [PlayerCount]

data Match = Match {
  locId   :: Location
, scores  :: Maybe [PlayerCount]
, players :: [PlayerCount]
} deriving (Show, Eq)



data Elimination = Double | Single deriving (Show, Eq, Ord)

-- | Create match shells for an elimination tournament
-- hangles walkovers and leaves the tournament in a stable initial state
eliminationOf :: Elimination -> PlayerCount -> [Match]
e `eliminationOf` np
  -- need 2 players for a tournament
  | np < 2 = []
  -- grand final rules fail if LB final is LBR1 (n=1) => GF in 2*n-1 == 1
  | np == 2 && e == Double = []
  -- 2 player Single elimination, a bit pointless, but that's how it looks
  | np == 2 = let l = Location { brac = Winners, rnd = 1, num = 1 }
    in [Match { locId = l, players = [1,2], scores = Nothing}]
  -- else, a single/double elim with at least 2 WB rounds happening
  | otherwise =
    let p = (ceiling . logBase 2 . fromIntegral) np
        np' = 2^p

        -- dummies map to WO markers
        markWO (x, y) = map (\x -> if x <= np then x else -1) [x,y]

        -- woWinners is easy to compute in a duel
        woWinners (x:y:[])
          | x == -1 = Just [0, 1] -- top player lost
          | y == -1 = Just [1, 0] -- top player won
          | otherwise = Nothing

        -- proceeding
        {-
        need to check if scores is a Just
        if it is, pipe it to winners?
        -}


        -- complete WBR1 by filling in -1 as WO markers for missing (np'-np) players
        makeWbR1 i = Match {locId = l, players = pl, scores = s} where
          l = Location { brac = Winners, rnd = 1, num = i }
          pl = markWO $ seeds p i
          s = woWinners pl
        wbr1 = map makeWbR1 [1..2^(p-1)]

        -- make WBR2 shells by using WBR1 results to propagate walkover winners
        makeWbR2 (r1m1, r1m2) = Match {locId = l, players = pl, scores = s} where
        --makeWbR2 (r1m1, r1m2) = Match {b = Winners, r = 2, m = i, p = [po, pe]}
          l = Location { brac = Winners, rnd = 2, num = r1m2.num `div` 2 }

          po = r1m1 winner if exists else 0
          pe = r1m2 winner if exists else 0

        wbr2 = map makeWbR2 $ take 2^(p-2) $ zip wbr1 (tail wbr1)

        {-
        -- any rounds after this point is not guaranteed to happen


        -- make LBR1 shells by using WBR1 results to propagate WO markers down
        makeLbR1 (r1m1, r1m2) = Match {b = Losers, r = 1, m = i, p = [po, pe]}
          where i = r1m2.m `div` 2
                po = r1m1 loser
                pe = r1m2 loser

        -- make LBR2 shells by using LBR1 results to propagate WO markers if 2x
        makeLbR2 (r1m1, r1m2)

        wbLoc = Location Winners
        lbLoc = Location Losers

        map ( . wbLoc r) [1..] -- result of outer gives a loc for round r
        -- can then map location to match possibly if we also include other info..


        -}

        {-
        emptyMatch l = Match { loc = l, players = [0,0], scores = Nothing}

        makeWbRound k = map makeWbMatch [1..2^(p-k)] where
          makeWbMatch i = emptyMatch $ Location { brac = Winners, rnd = k, offs = i } 

        wbRest = map makeWbRound [3..p]

        makeLbRound k = map makeLbMatch [1..(2^) $ p - 1 - (k+1) `div` 2] where
          makeLbMatch i = emptyMatch $ Location { brac = Losers, rnd = k, offs = i }

        gf1 = Location { brac = Losers, offs = 1, rnd = 2*p-1 }
        gf2 = Location { brac = Losers, offs = 1, rnd = 2*p }
        gfms = map emptyMatch [gf1, gf2]

        lbRest = (map makeLbRound [3..2*p-2]) ++ gfms

        -}
        in wbr1 ++ wbr2
