module Game.Tournament (
   -- * Duel helpers
     seeds             -- :: Int -> Int -> (Int, Int)
   , duelExpected      -- :: Int -> (Int, Int) -> Bool

   -- * Group helpers
   , inGroupsOf        -- :: Int -> Int -> [Group]
   , robin             -- :: Int -> [RobinRound]

   -- * Tournament helpers
   , tournament        -- :: Rules -> Size -> Tournament
   , score             -- :: MatchId -> Maybe [Score] -> Tournament -> Tournament

) where

import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase, readInt)
import Data.List (sort, sortBy, genericTake)
import Data.Ord (comparing)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isJust)
import Control.Monad.State --what? at least State constructor
import Data.Map (Map)
import System.IO.Unsafe (unsafePerformIO) -- while developing
import qualified Data.Map as Map

-- -----------------------------------------------------------------------------
--testor :: Tournament -> IO()
testor Tourney { matches = ms } = mapM_ print $ Map.assocs ms
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
  permute (x:xs@(_:_)) = x : last xs : init xs
  permute xs = xs -- not necessary, wont be called on length 1/2 lists
  rounds = genericTake (n'-1) $ iterate permute [1..n']
  notDummy (x,y) = all (<=n) [x,y]
  toPairs x =  genericTake m $ zip x (reverse x)

-- -----------------------------------------------------------------------------
-- Duel elimination

data Bracket = WB | LB deriving (Show, Eq, Ord)
data Round = R Int deriving (Show, Eq, Ord)
data Game = G Int deriving (Show, Eq, Ord)
data MatchId = MID Bracket Round Game deriving (Show, Eq, Ord)
--Note: instanceof Ord MatchId sorts by unequal Bracket, else unequal Round, else Game
gameNum :: MatchId -> Int -- convenience
gameNum (MID _ _ (G g)) = g

type Player = Int
type Score = Int
-- if scored, scored all at once - zip gives the correct association between scores and players
data Match = M [Player] (Maybe [Score]) deriving (Show, Eq)

--getScores :: Match -> Maybe [Int] -- convenience
--getScores (M _ scr) = scr


type Matches = Map MatchId Match
--showTournament t = mapM_ print $ Map.toList t

-- Ordered set of winners. Ordering is descending, i.e. head is the winner.
-- NB: more wins =/> better placement (LB player may have more wins than GF winner from WB for example).
type Wins = Int
type Results = [(Player, Wins)]
data Elimination = Single | Double deriving (Show, Eq, Ord)
data GroupSize = GS Int deriving (Show, Eq, Ord)
data Advancers = Adv Int deriving (Show, Eq, Ord)
data Rules = FFA GroupSize Advancers | Duel Elimination
type Size = Int

data Tournament = Tourney {
  size    :: Size
, rules   :: Rules
, matches :: Matches
, results :: Maybe Results
}

-- Called when final game's state was modified by scoreElimination/scoreFFA.
--finalize :: Tournament -> Tournament
--finalize t@{Tourney {results = rs}) = t { results = rx } where -- modify only results
--  rx = rs
  --GET LAST GAME, should be easy due to Ord instance on Match if used right..
  -- DE:
  --find winner => @1
  --find loser => #2
  --find LB final loser => #3
  --find  LB pre-final?/semi? 4-5 or 4 then 5-6?


-- helpers

-- these are rules agnostic
scores :: Match -> [Int]
scores (M pls Nothing) = replicate (length pls) 0
scores (M pls (Just scrs)) = map fst $ reverse $ sortBy (comparing snd) $ zip pls scrs

winner, loser :: Match -> Int
winner = head . scores
loser = last . scores

-- duel specific maybe exportable
--duelPower :: Tournament -> Int
--duelPower Tourney {size = np} = pow np

pow :: Int -> Int
pow = ceiling . logBase 2 . fromIntegral

woScores :: [Int] -> Maybe [Int]
woScores ps
  |  0 `elem` ps = Nothing
  | -1 `elem` ps = Just $ map (\x -> if x == -1 then 0 else 1) ps
  | otherwise    = Nothing


-- | Create match shells for an FFA elimination tournament.
-- Result comes pre-filled in with either top advancers or advancers `intersect` seedList.
-- This means what the player numbers represent is only fixed per round.
tournament :: Rules -> Size -> Tournament
tournament rs@(FFA (GS gs) (Adv adv)) np
  -- Enforce >2 players, >2 players per match, and >1 group needed.
  -- Not technically limiting, but: gs 2 <=> duel and 1 group <=> best of one.
  | np <= 2 = error "Need >2 players for an FFA elimination"
  | gs <= 2 = error "Need >2 players per match for an FFA elimination"
  | np <= gs = error "Need >1 group for an FFA elimination"
  | adv >= gs = error "Need to eliminate at least one player a match in FFA elimination"
  | adv <= 0 = error "Need >0 players to advance per match in a FFA elimination"
  | otherwise =
    --TODO: allow crossover matches when there are gaps intelligently..
    let minsize = minimum . map length
    --TODO: crossover matches?

        nextGroup g = leftover `inGroupsOf` gs where
          adv' = adv - (gs - minsize g) -- force zero non-eliminating matches
          adv'' = max adv' 1 -- but not if we only left 1 ^^ should still hold
          leftover = length g * adv''

        grps = takeWhile ((>1) . length) $ iterate nextGroup $ np `inGroupsOf` gs
        final = nextGroup $ last grps

        -- finally convert raw group lists to matches
        makeRound grp r = zipWith makeMatch grp [1..] where
          makeMatch g i = (MID WB (R r) (G i), M g Nothing)

        ms = Map.fromList $ concat $ zipWith makeRound (final : grps) [1..]
    in Tourney { size = np, rules = rs, matches = ms, results = Nothing }


-- | Create match shells for an elimination tournament
-- hangles walkovers and leaves the tournament in a stable initial state
tournament rs@(Duel e) np
  -- Enforce >2 players for a tournament. It is possible to extend to 2, but:
  -- 2 players Single <=> a bestempty of 1 match
  -- 2 players Double <=> a best of 3 match
  -- and grand final rules fail when LB final is R1 (p=1) as GF is then 2*p-1 == 1 ↯
  | np < 4 = error "Need >=4 competitors for an elimination tournament"

  -- else, a single/double elim with at least 2 WB rounds happening
  | otherwise = Tourney { size = np, rules = rs, matches = ms, results = Nothing } where
    p = pow np

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
    ms = if e == Single then wb else wb `Map.union` lb


-- | General score tournament function
-- Can eventually invoke the state monadic individual functions : )
-- TODO: make a strict version of this
score :: MatchId -> [Score] -> Tournament -> Tournament
score id sc trn@(Tourney {rules = Duel e, size = np, matches = ms}) =
  let msUpd = execState (scoreElimination np e id sc) ms
  in trn { matches = msUpd }
score _ _ _ {-id sc trn@(Tourney {rules = FFA _ _})-}= undefined

testcase :: IO ()
testcase = let
  upd :: MatchId -> [Score] -> State Tournament ()
  upd id sc = do
    t <- get
    put $ score id sc t
    return ()

  manip :: State Tournament ()
  manip = do
    --upd (MID WB (R 1) (G 1)) [1,0]
    upd (MID WB (R 1) (G 2)) [0,1]
    --upd (MID WB (R 1) (G 3)) [1,0]
    --upd (MID WB (R 1) (G 4)) [0,1]

    upd (MID WB (R 2) (G 1)) [1,0]
    upd (MID WB (R 2) (G 2)) [0,1]

    upd (MID LB (R 2) (G 1)) [1,0]
    upd (MID LB (R 3) (G 1)) [1,0]

    upd (MID WB (R 3) (G 1)) [1,0]
    upd (MID LB (R 4) (G 1)) [1,0]
    upd (MID LB (R 5) (G 1)) [0,1] -- gf1

    return ()

  in testor $ execState manip $ tournament (Duel Double) 5

-- Private helper to update a duel tournament's match map statefully.
-- Takes the player number, the (MatchId, Idx) pair from a progress fn to determine location.
playerInsert :: Maybe (MatchId, Int) -> Int -> State Matches (Maybe Match)
playerInsert Nothing _ = return Nothing
playerInsert (Just (kmid, idx)) x = do
  tmap <- get
  let (updated, tupd) = Map.updateLookupWithKey updFn kmid tmap
  put tupd
  return updated
    where updFn _ (M plsi _) = Just $ M plsm (woScores plsm) where
            plsm = if idx == 0 then [x, plsi !! 1] else [head plsi, x]

-- | Update a duel elimination tournament by passing in the Match, MatchID, and its
-- associated tournament. Returns an updated tournament with the winner propagated
-- to the next round, and the loser propagated to the loser bracket if applicable.
scoreElimination :: Int -> Elimination -> MatchId -> [Score] -> State Matches (Maybe Match)
scoreElimination np e id scrs
  = let p = pow np in do
    -- 0. verify match exists
    (Just (M pls _)) <- gets (Map.lookup id) -- short circuits if isNothing
    let m = M pls (Just scrs)

    -- 1. score given match
    modify $ Map.adjust (const m) id

    -- 2. move winner right
    let nprog = mRight True p id
    nres <- playerInsert nprog $ winner m

    -- 3. move loser to down if we were in winners
    let dprog = mDown p id
    dres <- playerInsert dprog $ loser m

    -- 4. WO Checks in LB (R {1,2})
    let r1checkp = woCheck p dprog dres
    -- a) Check if dres was scored in LBR1, place its winner in LBR2
    if isJust r1checkp
      then do
        let r1check = fromJust r1checkp
        lbr2res <- uncurry playerInsert $ r1check
        let r2check = woCheck p (fst r1check) lbr2res
        -- b) Check if lb2res was WO scored, place its winner in LBR3
        if isJust r2check
          then do
            uncurry playerInsert $ fromJust r2check
          else return Nothing
      else do
        let r2check = woCheck p nprog nres
        -- c) Check if mprog was WO scored in LBR2, place winner in LBR3
        if isJust r2check
          then do
            uncurry playerInsert $ fromJust r2check
          else return Nothing

    return $ Just m

    where
      -- given (power,) _prog and _res produce a new prog and its winner to send to playerInsert
      woCheck :: Int -> Maybe (MatchId, Int) -> Maybe Match -> Maybe (Maybe (MatchId, Int), Int)
      woCheck p (Just (mid, _)) (Just mi)
        | winner mi == 0 = Nothing
        | otherwise = Just (mRight False p mid, winner mi)
      woCheck _ _ _ = Nothing


      mRight :: Bool -> Int -> MatchId -> Maybe (MatchId, Int) -- winner moves right to this (MatchId, Position)
      mRight gf2Check p (MID br (R r) (G g))
        | r < 1 || g < 1 = error "bad MatchId"
        -- Nothing if last Match. NB: WB ends 1 round faster depending on e
        | r >= 2*p || (br == WB && (r > p || (e == Single && r == p))) = Nothing
        | br == LB  = Just (MID LB (R (r+1)) (G ghalf), pos)   -- standard LB progression
        | r == 2*p-1 && br == LB && gf2Check && maximum scrs == head scrs = Nothing
        | r == p    = Just (MID LB (R (2*p-1)) (G ghalf), 0)   -- WB winner -> GF1 path
        | otherwise = Just (MID WB (R (r+1)) (G ghalf), pos)   -- standard WB progression
          where
            ghalf = (g+1) `div` 2
            pos
              | br == WB = if odd g then 0 else 1         -- WB maintains standard alignment
              | r == 2*p-2 = 1                            -- LB final winner => bottom of GF
              | r == 2*p-1 = 0                            -- GF(1) winnner moves to the top [semantic]
              | (r == 1 && odd g) || (r > 1 && odd r) = 1 -- winner usually takes the bottom position
              | otherwise = if odd g then 0 else 1        -- normal progression only in even rounds + R1
              -- by placing winner on bottom consistently in odd rounds the bracket moves upward each new refill
              -- the GF(1) and LB final are special cases that give opposite results to the advanced rule above

      mDown :: Int -> MatchId -> Maybe (MatchId, Int) -- loser moves down to this (MatchId, Position)
      mDown p (MID br (R r) (G g))
        | br == LB || e == Single || r > p = Nothing
        | r == 1    = Just (MID LB (R 1) (G ghalf), pos)     -- WBR1 -> r=1 g/2 (LBR1 only gets input from WB)
        | otherwise = Just (MID LB (R ((r-1)*2)) (G g), pos) -- WBRr -> 2x as late per round in WB
          where
            ghalf = (g+1) `div` 2
            -- drop on top >R2, and <=2 for odd g to match bracket movement
            pos = if r > 2 || odd g then 0 else 1


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
