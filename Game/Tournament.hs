-- Based on the theory from http://clux.org/entries/view/2407

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

   , testcase
) where

import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase, readInt)
import Data.List (sort, sortBy, group, groupBy, genericTake)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isJust, fromMaybe, listToMaybe)
import Control.Monad.State --TODO: only what needed
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Control.Arrow ((&&&), second)
import System.IO.Unsafe (unsafePerformIO) -- while developing

-- -----------------------------------------------------------------------------
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
data Match = Match {
  players :: [Int]
, scores  :: Maybe [Score]
} deriving (Show, Eq)


type Matches = Map MatchId Match
--showTournament t = mapM_ print $ Map.toList t

-- Ordered set of winners. Ordering is descending, i.e. head is the winner.
-- NB: more wins =/> better placement (LB player may have more wins than GF winner from WB for example).
type Wins = Int
type Placement = Int
type Results = [(Player, Placement, Wins)]
--type Results = [(Player, Placement)]
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

testor :: Tournament -> IO ()
testor Tourney { matches = ms, results = rs } = do
  mapM_ print $ Map.assocs ms
  if isJust rs
    then do
      print "results:"
      mapM_ print $ fromJust rs
    else do print "no results"

-- throws if bad tournament
-- NB: tournament does not have updated Mathces, as this is called mid score
-- uses supplied extra argument for updated matches
makeResults :: Tournament -> Matches -> Maybe Results
makeResults (Tourney {rules = Duel e, size = np}) ms = rs where
  p = pow np
  --bf@(M _ bfsc) = fromJust $ Map.lookup (MID LB (R 1) (G 1)) ms bf in R 1?
  wbf@(Match _ wbfsc) = fromJust $ Map.lookup (MID WB (R p) (G 1)) ms
  rs = if e == Single && isJust wbfsc
    then Just $ scorify (winner wbf)
    else let
      gf1@(Match _ gf1sc) = fromJust $ Map.lookup (MID LB (R (2*p-1)) (G 1)) ms
      gf2@(Match _ gf2sc) = fromJust $ Map.lookup (MID LB (R (2*p)) (G 1)) ms
      in if e == Double && isJust gf2sc
        then Just $ scorify (winner gf2) 
        else if e == Double && isJust gf1sc && maximum (fromJust gf1sc) == head (fromJust gf1sc)
          then Just $ scorify (winner gf1)
          else Nothing
    
  -- maps (last bracket's) maxround to the tie-placement
  toPlacement :: Elimination -> Int -> Int
  toPlacement Double maxlbr = if metric <= 4 then metric else 2^(k+1) + 1 + oddExtra where
    metric = 2*p + 1 - maxlbr
    r = metric - 4
    k = (r+1) `div` 2
    oddExtra = if odd r then 0 else 2^k
  toPlacement Single maxr = if metric <= 2 then metric else 2^r + 1 where
    metric = p+1 - maxr
    r = metric - 2

  -- scoring function assumes winner has been calculated so all that remains is:
  -- sort by maximum (last bracket's) round number descending, possibly flipping winners
  scorify :: Int -> Results
  scorify w = map result placements where
    result (p, pos) = (p, pos, wi) where
      wi = fromMaybe 0 . listToMaybe . map snd . filter ((==p) . fst) $ wins

    wins = map (head &&& length)
      . group
      . sort
      . filter (>0)
      . Map.foldr ((:) . winner) []
      . Map.filter (all (>0) . players) $ ms

    placements = map (second (toPlacement e))
      . flipFirst
      . sortBy (flip compare `on` snd)
      . map (fst . head &&& foldr (max . snd) 0)
      . groupBy ((==) `on` fst)
      . sortBy (comparing fst)
      . filter ((>0) . fst) 
      . Map.foldrWithKey rfold [] $ ms

    rfold (MID br (R r) _) m acc =
      if e == Single || (e == Double && br == LB)
        then (++ acc) . map (id &&& const r) $ players m
        else acc

    flipFirst (x:y:xs) = if fst x == w then x : y : xs else y : x : xs
    flipFirst _ = error "<2 players in Match sent to flipFirst"
    -- if bronzeFinal then need to flip 3 and 4 possibly as well
    -- if this is scanned for loosely then we can score regardless of this being played
    -- otherwise we have a proper tie at 3 like normal



makeResults (Tourney {rules = FFA (GS _) (Adv _), size = _}) _ = undefined


-- helpers

-- these are rules agnostic
-- TODO: maybe export this?
sortedScores :: Match -> [Int]
sortedScores (Match pls Nothing) = replicate (length pls) 0
sortedScores (Match pls (Just scrs)) =
  map fst . reverse . sortBy (comparing snd) . zip pls $ scrs

-- these can be exported
winner, loser :: Match -> Int
winner = head . sortedScores
loser = last . sortedScores

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
          makeMatch g i = (MID WB (R r) (G i), Match g Nothing)

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
    makeWbR1 i = (l, Match pl s) where
      l = MID WB (R 1) (G i)
      pl = markWO $ seeds p i
      s = woScores pl

    -- make WBR2 shells by using paired WBR1 results to propagate walkover winners
    makeWbR2 ((_, m1), (l2, m2)) = (l, Match pl s) where
      l = MID WB (R 2) (G (gameNum l2 `div` 2))
      pl = map winner [m1, m2]
      s = woScores pl

    -- make LBR1 shells by using paired WBR1 results to propagate WO markers down
    makeLbR1 ((_, m1), (l2, m2)) = (l, Match pl s) where
      l = MID LB (R 1) (G (gameNum l2 `div` 2))
      pl = map loser [m1, m2]
      s = woScores pl

    -- make LBR2 shells by using LBR1 results to propagate WO markers if 2x
    makeLbR2 (l1, m1) = (l, Match pl Nothing) where
      l = MID LB (R 2) (G (gameNum l1))
      plw = winner m1
      pl = if odd (gameNum l) then [0, plw] else [plw, 0]

    -- remaining rounds empty
    emptyMatch l = (l, Match [0,0] Nothing)
    makeWbRound k = map makeWbMatch [1..2^(p-k)] where
      makeWbMatch i = emptyMatch $ MID WB (R k) (G i)

    makeLbRound k = map makeLbMatch [1..(2^) $ p - 1 - (k+1) `div` 2] where
      makeLbMatch i = emptyMatch $ MID LB (R k) (G i)

    -- construct matches
    wbr1 = map makeWbR1 [1..2^(p-1)]
    wbr1pairs = take (2^(p-2))
      $ filter (even . gameNum . fst . snd) $ zip wbr1 (tail wbr1)
    wbr2 = map makeWbR2 wbr1pairs
    lbr1 = map makeLbR1 wbr1pairs
    lbr2 = map makeLbR2 lbr1
    wbrest = concatMap makeWbRound [3..p]
    --TODO: bronze final special

    gf1 = MID LB (R (2*p-1)) (G 1)
    gf2 = MID LB (R (2*p)) (G 1)
    gfms = map emptyMatch [gf1, gf2]
    lbrest = concatMap makeLbRound [3..2*p-2]

    wb = Map.fromList $ wbr1 ++ wbr2 ++ wbrest
    lb = Map.fromList $ lbr1 ++ lbr2 ++ lbrest ++ gfms
    ms = if e == Single then wb else wb `Map.union` lb


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
    upd (MID LB (R 5) (G 1)) [1,0] -- gf1

    return ()

  in testor $ execState manip $ tournament (Duel Double) 5

-- | Score a match in a tournament and propagate winners/losers.
-- TODO: make a strict version of this
-- TODO: documentation absorb the individual functions?
-- TODO: test if MID exists, subfns throw if lookup fail
score :: MatchId -> [Score] -> Tournament -> Tournament
score id sc trn@(Tourney {rules = Duel e, size = np, matches = ms}) =
  let msUpd = execState (scoreDuel (pow np) e id sc) ms
      rsUpd = makeResults trn msUpd
  in trn { matches = msUpd, results = rsUpd }
score _ _ _ {-id sc trn@(Tourney {rules = FFA _ _, matches = ms}) -}= scoreFFA

-- | Update the scores of a duel in an elimination tournament.
-- Returns an updated tournament with the winner propagated to the next round,
-- and the loser propagated to the loser bracket if applicable.
scoreDuel :: Int -> Elimination -> MatchId -> [Score] -> State Matches (Maybe Match)
scoreDuel p e mid scrs = do
  -- 0. get involved players / verify match exists
  (Just (Match pls _)) <- gets (Map.lookup mid) -- NB: throws if invalid MID
  let m = Match pls (Just scrs)

  -- 1. score given match
  modify $ Map.adjust (const m) mid

  -- 2. move winner right
  let nprog = mRight True p mid
  nres <- playerInsert nprog $ winner m

  -- 3. move loser to down if we were in winners
  let dprog = mDown p mid
  dres <- playerInsert dprog $ loser m

  -- 4. check if loser needs WO from LBR1
  let dprog2 = woCheck p dprog dres
  uncurry playerInsert $ fromMaybe (Nothing, 0) dprog2

  -- 5. check if winner needs WO from LBR2
  let nprog2 = woCheck p nprog nres
  uncurry playerInsert $ fromMaybe (Nothing, 0) nprog2

  return $ Just m

  where
    -- insert player x into list index idx of mid's players, and woScore it
    -- progress result determines location and must be passed in as fst arg
    playerInsert :: Maybe (MatchId, Int) -> Int -> State Matches (Maybe Match)
    playerInsert Nothing _ = return Nothing
    playerInsert (Just (mid, idx)) x = do
      tmap <- get
      let (updated, tupd) = Map.updateLookupWithKey updFn mid tmap
      put tupd
      return updated
        where updFn _ (Match plsi _) = Just $ Match plsm (woScores plsm) where
                plsm = if idx == 0 then [x, last plsi] else [head plsi, x]

    -- given tourney power, progress results, and insert results, of previous
    -- if it was woScored in playerInsert, produce new (progress, winner) pair
    woCheck :: Int -> Maybe (MatchId, Int) -> Maybe Match -> Maybe (Maybe (MatchId, Int), Int)
    woCheck p (Just (mid, _)) (Just mi)
      | winner mi == 0 = Nothing
      | otherwise = Just (mRight False p mid, winner mi)
    woCheck _ _ _ = Nothing

    -- right progress fn: winner moves right to (MatchId, Position)
    mRight :: Bool -> Int -> MatchId -> Maybe (MatchId, Int)
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

    -- down progress fn : loser moves down to (MatchId, Position)
    mDown :: Int -> MatchId -> Maybe (MatchId, Int)
    mDown p (MID br (R r) (G g))
      | br == LB || e == Single || r > p = Nothing
      | r == 1    = Just (MID LB (R 1) (G ghalf), pos)     -- WBR1 -> r=1 g/2 (LBR1 only gets input from WB)
      | otherwise = Just (MID LB (R ((r-1)*2)) (G g), pos) -- WBRr -> 2x as late per round in WB
        where
          ghalf = (g+1) `div` 2
          -- drop on top >R2, and <=2 for odd g to match bracket movement
          pos = if r > 2 || odd g then 0 else 1

--scoreFFA ::
scoreFFA = undefined


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
