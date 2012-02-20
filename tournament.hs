import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase, readInt)
import Data.List (sort, splitAt)
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
    0 ->  2^(n-k)
    _ -> let bstr = reverse $ showIntAtBase 2 intToDigit (i - 2*r) ""
             nr = fst $ readInt 2 (`elem` "01") digitToInt bstr !! 0
         in 2^(n-k-1) + nr `shiftL` (n - length bstr)

-- Compute both the player seeds (in order) for a duel elimiation match
-- i is the match number (in R1B1) and n is the power of the tournament
-- Well-defined for n > 0 and 0 < i <= 2^(n-1)
seeds :: Int -> Int -> (Int, Int)
seeds n i = let evn = evenSeed n i in (1 - evn + 2^n, evn)

-- Splits a numer of players into groups of as close to equal sum as possible
-- When groupsize is even and s | n, the result is perfectly fair
inGroupsOf :: Int -> Int -> [[Int]]
0 `inGroupsOf` _ = []
n `inGroupsOf` s =
  let ngrps = ceiling $ fromIntegral n / fromIntegral s
      s' = s - head (filter (\x -> n > ngrps*(s-1-x)) [0..]) -- reduce s if too high to fit
      n' = ngrps*s' -- n if filled groups != n (10 inGroupsOf 4 uses n' = 12)
      nppg = s' `div` 2 -- num pair per group
      np = nppg * ngrps  -- total num pairs
      ps = take np $ zip [1..] [n', n'-1..] -- all the pairs
      rem = [np+1, np+2 .. n'-np] -- e in [1..n'] \\ ps
      makeGroup i = single ++ pairs
        where pairs = concat [[x,y] | (x,y) <- ps, x `elem` [i, i+ngrps .. i+np]]
              single = if length rem >= i then [rem !! (i-1)] else []
      in map (sort . filter (<=n) . makeGroup) [1..ngrps]

-- Round robin scheduling algorithm
-- returns a list of rounds (where a round is a list of pairs)
robin :: Int -> [[(Int, Int)]]
robin n =
  let n' = if odd n then n+1 else n
      m = n' `div` 2 -- matches per round
      permute (x:xs) = x : (last xs) : (init xs)
      rounds = take (n'-1) $ iterate permute [1..n']
      notDummy (x,y) = all (<=n) [x,y]
      toPairs x =  take m $ zip x (reverse x)
      in map (filter notDummy . toPairs) rounds

-- Create match shells for an elimination tournament
-- allows brackets to be 1 or 2 at the moment
elimination np brackets
  -- need 2 players for a tournament
  | np < 2 = []
  -- grand final rules fail if LB final is LBR1 (n=1) => GF in 2*n-1 == 1
  | np <= 2 && brackets > 1 = []
  | brackets > 2 = error "Triple (or greater) elimination not implemented"
  | otherwise =
    let n = (ceiling . logBase 2 . fromIntegral) np
        num = 2^n
        matches = []

        in matches




