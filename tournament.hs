import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase, readInt)
import Data.List (sort, sortBy)
--import Data.Function (on)
import Data.Ord (comparing)
import Bits (shiftL)

-- A duel has valid seeding placement if the following conditions hold
-- Can use this to make a measure of how good the seeding was in retrospect
duelValid :: Int -> (Int, Int) -> Bool
duelValid n (a, b) = odd a && even b && a + b == 1 + 2^n

--TODO: should somehow ensure 0 < i <= 2^(n-1) in the next fns

-- Duel tournament seeds helper
evenSeed :: Int -> Int -> Int
evenSeed n i =
  let (k, r) = ((floor . logBase 2 . fromIntegral) i, i - 2^k) in
  case r of
    0 -> 2^(n-k)
    _ -> let bstr = reverse $ showIntAtBase 2 intToDigit (i - 2*r) ""
             nr = fst $ readInt 2 (`elem` "01") digitToInt bstr !! 0
         in 2^(n-k-1) + nr `shiftL` (n - length bstr)

-- Compute both the player seeds (in order) for a duel elimiation match
-- i is the match number (in R1B1) and n is the power of the tournament
-- Well-defined for n > 0 and 0 < i <= 2^(n-1)
seeds :: Int -> Int -> (Int, Int)
seeds n i = let e = evenSeed n i in (1 - e + 2^n, e)

-- Splits a numer of players into groups of as close to equal sum as possible
-- When groupsize is even and s | n, the result is perfectly fair
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
  makeGroup i = pairs ++ left where
    pairs = concat [[x,y] | (x,y) <- ps, x `elem` [i, i+ngrps .. i+np]]
    left = if length rem >= i then [rem !! (i-1)] else []


-- Round robin scheduling algorithm
-- returns a list of rounds (where a round is a list of pairs)
robin :: Int -> [[(Int, Int)]]
robin n = map (filter notDummy . toPairs) rounds where
  n' = if odd n then n+1 else n
  m = n' `div` 2 -- matches per round
  permute (x:xs) = x : (last xs) : (init xs)
  rounds = take (n'-1) $ iterate permute [1..n']
  notDummy (x,y) = all (<=n) [x,y]
  toPairs x =  take m $ zip x (reverse x)


data Bracket = Losers | Winners deriving (Show, Eq, Ord)


type Round = Int
--data Round n = [1..2^n], although wont work as n is a type variable
--if that is possible you an also make Offset depend on Round, perhaps nice


type Offset = Int
data Location = Location Bracket Round Offset deriving (Show, Eq)
{-location can be used as the lookup key in an assoc. list
then scorer function can do lookups and update..
although this becomes hard when we have to modify the entire list
either we need a monad for this or we need to subdivide the lists into
lists of rounds so that one round can be quickly updated..
stateful updates should ONLY be necessary by an updater function however
NOT the creation one, cause that should make round per round anyway..
-}

--updateScore :: Location -> Scores -> State Match
-- perhaps it should work on an association list of Location Match


type Player = Int
--type Score = Maybe Int
{- Score cannot simply have (Int | BYE, Score | UNFILLED) because then
it is hard to determine winners in general:
first have to look at #BYEs, then if sufficient let one through, otherwise
check if scores are filled in, order non BYE's by score

maybe instead of dealing with Bools that dont help the computation
keep scores in the PlayerScore pair, and have them Maybe
and once all filled in, fill in the outer (Maybe Results) in Match
where results == ordered results (=Maybe [Int])
that way functions know what's played

but one thing that is iffy, how to deal with Maybe in databases like mongo?
maybe we can serialize them in another way?
-}


{-
get winners: if -1 `elem` (map fst x) then if fst . head == -1 etc..
or work on (fst . unzip) players like before
finally just set victors there if WO [note WO scores -1, draws random?]
-}

data PlayerScore = PlayerScore Int (Maybe Int) deriving (Show, Eq)


type Players = [Int]
data Match = Match Location Players (Maybe Score) deriving (Show)

data Elimination = Double | Single deriving (Show, Eq, Ord)

-- Create match shells for an elimination tournament
-- hangles walkovers and leaves the tournament in a stable initial state
eliminationOf :: Elimination -> Int -> [Match]
e `eliminationOf` np
  -- need 2 players for a tournament
  | np < 2 = []
  -- grand final rules fail if LB final is LBR1 (n=1) => GF in 2*n-1 == 1
  | np <= 2 && e == Double = []
  | otherwise =
    let p = (ceiling . logBase 2 . fromIntegral) np
        np' = 2^p
        nm = 2^(p-1)

        markWO pl = take 2 $ (filter ((<= np) . fst) pl) ++ [(-1, Nothing)]
        winners pss = pss' where
          --length $ filter (>=0) pss
          -1 `elemIndices`
          take the complement as winners in duel.. harder in ffa..

        --winners pss = zip ps' ss' where
        --ps' = -1 `elem` ps = if head ps == -1 then last ps else head ps
        --  where (ps, ss) = unzip pss

        {-
        -- complete WBR1 by filling in -1 as WO markers for missing (np'-np) players
        makeWbR1 i = Match {b = Winners, r = 1, m = i, p = pl, w = wa}
          where pl = markWO $ seeds p i
                wa = winners pl
        wbr1 = map makeWbR1 [1..nm]

        -- make WBR2 shells by using WBR1 results to propagate walkover winners
        makeWbR2 (r1m1, r1m2) = Match {b = Winners, r = 2, m = i, p = [po, pe]}
          where i = r1m2.m `div` 2
                po = r1m1 winner if exists else 0
                pe = r1m2 winner if exists else 0
        wbr2 = map makeWbR2 $ take 2^(p-2) $ zip wbr1 (tail wbr1)

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
        in []

