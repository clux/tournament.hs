module Game.Tournament (
   -- * Duel helpers
     seeds             -- :: Int -> Int -> (Int, Int)
   , duelExpected      -- :: Int -> (Int, Int) -> Bool

   -- * Group helpers
   , inGroupsOf        -- :: Int -> Int -> [Group]
   , robin             -- :: Int -> [RobinRound]

   -- * Duel eliminationOf
   , duelElimination   -- :: Elimination -> Int -> Tournament
   , scoreElimination  -- :: Tournament -> MatchId -> Match -> Tournament

   -- * FFA Elimination
   , ffaElimination    -- :: Int -> Int -> Int -> Tournament

) where

import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase, readInt)
import Data.List (sort, sortBy, genericTake)
import Data.Ord (comparing)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isJust)
import Data.Map (Map)
import qualified Data.Map as Map

-- -----------------------------------------------------------------------------
-- Duel Helperstestor n = mapM_ print (Single `eliminationOf` n)
-- Based on the theory from http://clux.org/entries/view/2407
-- TODO should somehow ensure 0 < i <= 2^(p-1) in the next fn

-- | Computes both the player seeds (in order) for a duel elimiation match.
-- The first argument, p, is the power of the tournament,
-- and the second, i, is the match number.
-- Well-defined for p > 0 and 0 < i <= 2^(p-1)
seeds :: Int -> Int -> (Int, Int)
seeds p i = (1 - lastSeed + 2^p, lastSeed) where
  lastSeed = let (k, r) = ((floor . logBase 2 . fromIntegral) i, i - 2^k) in
    case r of
      0 -> 2^(p-k)
      _ -> 2^(p-k-1) + nr `shiftL` (p - length bstr) where
        bstr = reverse $ showIntAtBase 2 intToDigit (i - 2*r) ""
        nr = fst $ head $ readInt 2 (`elem` "01") digitToInt bstr

-- | Check if the 3 criteria for perfect seeding holds for the current
-- power and seed pair arguments.
-- This can be used to make a measure of how good the seeding was in retrospect
duelExpected :: Integral a => a -> (a, a) -> Bool
duelExpected n (a, b) = odd a && even b && a + b == 1 + 2^n

-- -----------------------------------------------------------------------------
-- Group helpers
--type Group = [Int]

-- | Splits a numer of players into groups of as close to equal seeding sum
-- as possible. When groupsize is even and s | n, the seed sum is constant.
inGroupsOf :: Int -> Int -> [[Int]]
0 `inGroupsOf` _ = []
n `inGroupsOf` s = map (sort . filter (<=n) . makeGroup) [1..ngrps] where
  ngrps = ceiling $ fromIntegral n / fromIntegral s
  s' = s - head (filter (\x -> n > ngrps*(s-1-x)) [0..]) -- reduce s if unfillable
  n' = ngrps*s' -- n if filled groups != n (10 inGroupsOf 4 uses n' = 12)
  npairs = (s' `div` 2) * ngrps
  pairs = zip [1..npairs] [n', n'-1..]
  leftovers = [npairs+1, npairs+2 .. n'-npairs] -- [1..n'] \\ e in pairs
  makeGroup i = leftover ++ concatMap (\(x,y) -> [x,y]) gpairs where
    gpairs = filter ((`elem` [i, i+ngrps .. i+npairs]) . fst) pairs
    leftover = take 1 $ drop (i-1) leftovers

-- | Round robin schedules a list of n players and returns
-- a list of rounds (where a round is a list of pairs). Uses
-- http://en.wikipedia.org/wiki/Round-robin_tournament#Scheduling_algorithm
robin :: Integral a => a -> [[(a,a)]]
robin n = map (filter notDummy . toPairs) rounds where
  n' = if odd n then n+1 else n
  m = n' `div` 2 -- matches per round
  rounds = genericTake (n'-1) $ iterate robinPermute [1..n']
  notDummy (x,y) = all (<=n) [x,y]
  toPairs x =  genericTake m $ zip x (reverse x)

robinPermute :: [a] -> [a]
robinPermute [] = []
robinPermute [x] = [x]
robinPermute (x:xs) = x : last xs : init xs -- know not null xs

-- -----------------------------------------------------------------------------
-- Duel elimination

data Bracket = WB | LB deriving (Show, Eq, Ord)
data Round = R Int deriving (Show, Eq, Ord)
data Game = G Int deriving (Show, Eq, Ord)
data MatchId = MID Bracket Round Game deriving (Show, Eq, Ord)
--Note: instanceof Ord MatchId sorts by unequal Bracket, else unequal Round, else Game
gameNum (MID _ _ (G g)) = g -- convenience

type Players = [Int]
type Scores = Maybe [Int]
scores (M _ scr) = scr -- convenience

data Match = M [Int] (Maybe [Int]) deriving (Show, Eq)
type Tournament = Map MatchId Match
-- could make Tournament: data Tournament = Tournament (Map MatchId Match)
-- then instanceof Show Tournament where show = showTournament
-- but then you always have to unwrap it to get the list
showTournament t = mapM_ print $ Map.toList t

data Elimination = Single | Double deriving (Show, Eq, Ord)

results :: Match -> [Int]
results (M pls Nothing) = replicate (length pls) 0
results (M pls (Just scrs)) = map fst $ reverse $ sortBy (comparing snd) $ zip pls scrs

winner :: Match -> Int
winner = head . results

loser :: Match -> Int
loser = last . results

woScores :: [Int] -> Maybe [Int]
woScores ps
  |  0 `elem` ps = Nothing
  | -1 `elem` ps = Just $ map (\x -> if x == -1 then 0 else 1) ps
  | otherwise    = Nothing

-- | Create match shells for an elimination tournament
-- hangles walkovers and leaves the tournament in a stable initial state
duelElimination :: Elimination -> Int -> Tournament
duelElimination etype np
  -- Enforce >2 players for a tournament. It is possible to extend to 2, but:
  -- 2 players Single <=> a bestempty of 1 match
  -- 2 players Double <=> a best of 3 match
  -- and grand final rules fail when LB final is R1 (p=1) as GF is then 2*p-1 == 1 ↯
  | np < 4 = error "Need >=4 competitors for an elimination tournament"

  -- else, a single/double elim with at least 2 WB rounds happening
  | otherwise = if etype == Single then wb else wb `Map.union` lb where
    p = (ceiling . logBase 2 . fromIntegral) np

    -- complete WBR1 by filling in -1 as WO markers for missing (np'-np) players
    markWO (x, y) = map (\a -> if a <= np then a else -1) [x,y]
    makeWbR1 i = (l, M pl s) where
      l = MID WB (R 1) (G i)
      pl = markWO $ seeds p i
      s = woScores pl

    -- make WBR2 shells by using paired WBR1 results to propagate walkover winners
    makeWbR2 ((_, m1), (l2, m2)) = (l, M pl s) where
      l = MID WB (R 2) (G (gameNum l2 `div` 2))
      pl = map winner [m1, m2]
      s = woScores pl

    -- make LBR1 shells by using paired WBR1 results to propagate WO markers down
    makeLbR1 ((_, m1), (l2, m2)) = (l, M pl s) where
      l = MID LB (R 1) (G (gameNum l2 `div` 2))
      pl = map loser [m1, m2]
      s = woScores pl

    -- make LBR2 shells by using LBR1 results to propagate WO markers if 2x
    makeLbR2 (l1, m1) = (l, M pl Nothing) where
      l = MID LB (R 2) (G (gameNum l1))
      plw = winner m1
      pl = if odd (gameNum l) then [0, plw] else [plw, 0]

    -- remaining rounds empty
    emptyMatch l = (l, M [0,0] Nothing)
    makeWbRound k = map makeWbMatch [1..2^(p-k)] where
      makeWbMatch i = emptyMatch $ MID WB (R k) (G i)

    makeLbRound k = map makeLbMatch [1..(2^) $ p - 1 - (k+1) `div` 2] where
      makeLbMatch i = emptyMatch $ MID LB (R k) (G i)

    -- construct matches
    wbr1 = map makeWbR1 [1..2^(p-1)]
    wbr1pairs = take (2^(p-2))
      $ filter (\(_ , (l, _)) -> even (gameNum l))
      $ zip wbr1 (tail wbr1)
    wbr2 = map makeWbR2 wbr1pairs
    lbr1 = map makeLbR1 wbr1pairs
    lbr2 = map makeLbR2 lbr1
    wbrest = concatMap makeWbRound [3..p]

    gf1 = MID LB (R (2*p-1)) (G 1)
    gf2 = MID LB (R (2*p)) (G 1)
    gfms = map emptyMatch [gf1, gf2]
    lbrest = concatMap makeLbRound [3..2*p-2]

    wb = Map.fromList $ wbr1 ++ wbr2 ++ wbrest
    lb = Map.fromList $ lbr1 ++ lbr2 ++ lbrest ++ gfms

-- Find the next Id from current (and power of tournament).
-- Assumes the MatchId is valid, and not the last in WB
progressNext :: Int -> Elimination -> MatchId -> Maybe MatchId
progressNext p e (MID br (R r) (G g))
  -- Nothing from invalid combinations. NB: WB ends 1 round faster depending on e
  | (r < 1 || r >= 2*p) || (br == WB && (r > p || (e == Single && r == p))) = Nothing
  | br == LB  = Just $ MID LB (R (r+1)) (G ghalf)   -- standard LB progression
  | r == p    = Just $ MID LB (R (2*p-1)) (G ghalf) -- WB winner -> GF1 path
  | otherwise = Just $ MID WB (R (r+1)) (G ghalf)   -- standard WB progression
    where ghalf = g+1 `div` 2

-- Find the drop Id from the current to LB (only makes sense for Double Elimination)
progressDrop :: Int -> MatchId -> Maybe MatchId
progressDrop p (MID br (R r) (G g))
  | r < 1 || r > p = Nothing
  | r == 1    = Just $ MID LB (R 1) (G ghalf)     -- WBR1 -> r=1 g/2 (LBR1 only gets input from WB)
  | otherwise = Just $ MID LB (R ((r-1)*2)) (G g) -- WBRr -> 2x as late per round in WB
    where ghalf = g+1 `div` 2


-- | Update a duel elimination tournament by passing in the Match, MatchID, and its
-- associated tournament. Returns an updated tournament with the winner propagated
-- to the next round, and the loser propagated to the loser bracket if applicable.
scoreElimination :: Tournament -> MatchId -> Match -> Tournament
scoreElimination t id@(MID br (R r) (G g)) m@(M pls (Just scrs)) = t''' where
  --could optimize these 2 away by passing in these two params, but premature pointlessness
  e = if Map.null $ Map.filterWithKey (\(MID bri _ _) _ -> bri == LB) t
          then Single else Double
  np = (2*) $ Map.size $ Map.filterWithKey (\(MID bri (R ri) _) _ -> bri == WB && ri == 1) t
  p = (ceiling . logBase 2 . fromIntegral) np

  updatePlayer :: MatchId -> Int -> Int -> Tournament -> (Maybe Match, Tournament)
  updatePlayer kmid idx replaced tmap = Map.updateLookupWithKey adjustNext kmid tmap where
    adjustNext _ (M plsi _) = Just (M plsm (woScores plsm)) where
      plsm = zipWith (\o i -> if i == idx then replaced else o) plsi [0,1]

  -- 1. score given match
  t' = Map.adjust (const m) id t

  -- 2. move winner to next round if not a final
  Just nextMid = progressNext p e id
  nextPos
    | br == WB = if odd g then 0 else 1         -- WB game maintains standard alignment always
    | r == 2*p-2 = 1                            -- LB final winner => bottom of GF
    | r == 2*p-1 = 0                            -- GF(1) winnner should move to the top [semantic only]
    | (r == 1 && odd g) || (r > 1 && odd r) = 1 -- winner usually takes the bottom position
    | otherwise = if odd g then 0 else 1        -- normal progression in even rounds + R1 only
    -- by placing winner on bottom consistently in odd rounds the bracket moves upward each new refill
    -- the GF(1) and LB final are special cases that give opposite results to the advanced rule above

  -- update next match if we're not at the end:
  validWbNext = (e == Single && r < p) || (e == Double && r <= p)
  deFinalIsDouble = maximum scrs /= head scrs
  validNext = (br == WB && validWbNext) || (br == LB && (r < 2*p-1 || deFinalIsDouble))

  -- adjust and return value to see if propagation of woMarkers are necessary
  -- TODO: if validNext and don't assume nextVal isJust
  (Just nextVal, t'') = updatePlayer nextMid nextPos (winner m) t'


  -- 3. move loser to down if we were in winners
  Just dropMid = progressDrop p id
  -- drop on top after initial round, and initially for odd g to match bracket movement
  dropPos = if r > 2 || odd g then 0 else 1

  -- update loser in LB if we're not in LB and it's not Single Elim:
  validDrop = br == WB && e == Double

  -- adjust and return value to see if propagation of woMarkers are necessary
  -- TODO: if validDrop and don't assume dropVal isJust
  (Just dropVal, t''') = updatePlayer dropMid dropPos (loser m) t''

  -- 4. Check for WO markers in LBR1 and LBR2
  checkLbR1 = inLbR 1 dropMid && (isJust . scores) dropVal
  checkLbR2 = (inLbR 2 dropMid && (isJust . scores) dropVal)
           || (inLbR 2 nextMid  && (isJust . scores) nextVal)
  inLbR ro (MID bri (R ri) _) = bri == LB && ri == r

  {-
  howto do 4 nicely??
  if checkLbR1

  -}


-- conditions for whether a Tournament is finished: TODO: factor to new fn?
-- NB: if LB advancing player wins GF(1) a GF(2) is necessary
{-deFinalOneWon = etype == Double && br == LB && r == 2*n-1 && not deFinalIsDouble
deFinalTwoWon = etype == Double && br == LB && r == 2*n
seFinalWon = etype == Single && br == WB && r == n
needFinalize = seFinalWon || deFinalOneWon || deFinalTwoWon
-}

-- | Checks if a Tournament is valid
{-
PERHAPS BETTER:
WB: always has np (rounded to nearest power) - 1 matches -- i.e. np = 2^p some p > 1
LB: always has 2*[num_wb_matches - 2^(p-1) + 1] -- i.e. minus the first round's matches but plus two finals
tournamentValid :: Tournament -> Bool
tournamentValid t =
  let (wb, lb) = partition ((== WB) . brac . locId) r
      roundRightWb k = rightSize && uniquePlayers where
        rightSize = 2^(p-k) == length $ filter ((== k) . rnd . locId) wb
        uniquePlayers =
      rountRightLb k = rightSize && uniquePlayers where
        rightSize = 2^(p - 1 - (k+1) `div` 2) == length $ filter ((== k) . rnd . locId) lb

  in all $ map roundRightWb [1..2^p]
-}

-- | Create match shells for an FFA elimination tournament.
-- Result comes pre-filled in with either top advancers or advancers `intersect` seedList.
-- This means what the player numbers represent is only fixed per round.
ffaElimination :: Int -> Int -> Int -> Tournament
ffaElimination gs adv np
  -- Enforce >2 players, >2 players per match, and >1 group needed.
  -- Not technically limiting, but: gs 2 <=> duel and 1 group <=> best of one.
  | np <= 2 = error "Need >2 players for an FFA elimination"
  | gs <= 2 = error "Need >2 players per match for an FFA elimination"
  | np <= gs = error "Need >1 group for an FFA elimination"
  | adv >= gs = error "Need to eliminate at least one player a match in FFA elimination"
  | adv <= 0 = error "Need >0 players to advance per match in a FFA elimination"
  | otherwise =
    let minsize = minimum . map length

        nextGroup g = leftover `inGroupsOf` gs where
          adv' = adv - (gs - minsize g) -- force zero non-eliminating matches
          adv'' = max adv' 1 -- but not if we only left 1 ^^ should still hold
          leftover = length g * adv''

        grps = takeWhile ((>1) . length) $ iterate nextGroup $ np `inGroupsOf` gs
        final = nextGroup $ last grps

        -- finally convert raw group lists to matches
        makeRound grp r = zipWith makeMatch grp [1..] where
          makeMatch g i = (MID WB (R r) (G i), M g Nothing)

    in Map.fromList $ concat $ zipWith makeRound (grps ++ [final]) [1..]
