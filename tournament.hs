import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase, readInt)
import Data.List (sort, splitAt)
import Bits (shiftL)

-- A duel has valid seeding placement if the following conditions hold
-- Can use this to make a measure of how good the seeding was in retrospect
duelValid :: Int -> (Int, Int) -> Bool
duelValid n (a, b) = odd a && even b && a + b == 1 + 2^n

--TODO: should somehow ensure 2^n is not less than i in the next fns

-- Compute the even seed for a 2 player tournament match of power n
-- Deterministic for n > 0 and 0 < i <= 2^n
evenSeed :: Int -> Int -> Int
evenSeed n i =
  let (k, r) = ((floor . logBase 2 . fromIntegral) i, i - 2^k) in
  case r of
    0 ->  2^(n-k)
    _ -> let bstr = reverse $ showIntAtBase 2 intToDigit (i - 2*r) ""
             nr = fst $ readInt 2 (`elem` "01") digitToInt bstr !! 0
         in 2^(n-k-1) + nr `shiftL` (n - length bstr)

-- Compute both the player seeds in order for a 2 player tournament match
seeds :: Int -> Int -> (Int, Int)
seeds n i = let evn = evenSeed n i in (1 - evn + 2^n, evn)


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
robin :: Int -> [[(Int, Int)]] -- list of lists of seed pairs returned
robin n =
  let n' = if odd n then n+1 else n
      players = [1..n'] -- last is a dummy when odd n
      r = n'-1 -- num rounds
      m = n' `div` 2 -- matches per round
      permute (x:xs) = x : (last xs) : (init xs)
      rounds = take r $ iterate permute players
      notDummy (x,y) = all (<=n) [x,y]
      toPairs x =  take m $ zip x (reverse x)
      in map (filter notDummy . toPairs) rounds

