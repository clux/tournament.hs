{-# LANGUAGE PatternGuards, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Game.Tournament
-- Copyright   :  (c) Eirik Albrigtsen 2012
-- License     :  GPL-style
-- Maintainer  :  Eirik <clux> Albrigtsen
-- Stability   :  unstable
--
-- Tournament construction and maintenance including competition based structures and helpers.
--
-- For simple uses of the the basic building blocks, qualified or partial imports is recommended.
-- However, due to the large number of simple types needed to operate the main tournament
-- machinery, a raw import into a dedicated outside-world interfacing helper file is recommended.
--
-- > import Game.Tournament
--
-- The Tournament structure contain a Map of 'MatchId' -> 'Match' for its internal
-- representation and the 'MatchId' keys are the location in the Tournament.

-- TODO: This structure is meant to encapsulate this structure to ensure internal consistency,
-- but hopefully in such a way it can be safely serialized to DBs.
-----------------------------------------------------------------------------

module Game.Tournament (
   -- * Building Block A: Duel helpers
     seeds
   , duelExpected

   -- * Building Block B: Group helpers
   , groups
   , robin

   -- * Tournament Types
   , GameId(..)
   , Elimination(..)
   , Bracket(..)
   , Rules(..)
   , Results

   , Result  -- no constructor
   , player
   , placement
   , wins
   , total

   , Size

   , Tournament -- no constructor
   , Score

   --, Game(..)
   --, Player

   --, Games

   -- * Tournament Interface
   , tournament
   , score
   , count

   -- -* Match Inspection
   --, scores
   --, winner
   --, loser

   , testcase
) where

import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase, readInt)
import Data.List (sort, sortBy, group, groupBy, genericTake)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isJust, fromMaybe)
import Control.Monad (when)
import Control.Monad.State (State, get, put, modify, execState, gets)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Control.Arrow ((&&&), second)
import System.IO.Unsafe (unsafePerformIO) -- while developing
import Prelude hiding (round)

-- -----------------------------------------------------------------------------
-- TODO should somehow ensure 0 < i <= 2^(p-1) in the next fn
-- | Duel tournaments is based on the theory from <http://clux.org/entries/view/2407>

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
-- Fixes the number of groups as ceil $ n / s, but will reduce s when all groups not full.
groups :: Int -> Int -> [[Int]]
groups 0 _ = []
groups s n = map (sort . filter (<=n) . makeGroup) [1..ngrps] where
  ngrps = ceiling $ fromIntegral n / fromIntegral s

  -- find largest 0<gs<=s s.t. even distribution => at least one full group, i.e. gs*ngrps - n < ngrps
  gs = until ((< ngrps + n) . (*ngrps)) (subtract 1) s

  modl = ngrps*gs -- modl may be bigger than n, e.e. groups 4 10 has a 12 model
  npairs = ngrps * (gs `div` 2)
  pairs = zip [1..npairs] [modl, modl-1..]
  leftovers = [npairs+1, npairs+2 .. modl-npairs] -- [1..modl] \\ e in pairs
  makeGroup i = leftover ++ concatMap (\(x,y) -> [x,y]) gpairs where
    gpairs = filter ((`elem` [i, i+ngrps .. i+npairs]) . fst) pairs
    leftover = take 1 . drop (i-1) $ leftovers

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

-- | The location of a game is written as to simulate the classical shorthand WBR2,
-- but includes additionally the game number for complete positional uniqueness.
--
-- A 'Single' elimination final will have the unique identifier
--
-- > let wbf = GameId WB p 1
--
-- where 'p == count t WB'.
data GameId = GameId {
  bracket :: Bracket
, round   :: Int
, game    :: Int
} deriving (Show, Eq, Ord)

-- | Duel Tournament option.
--
-- 'Single' elimation is a standard power of 2 tournament tree,
-- wheras 'Double' elimination grants each loser a second chance in the lower bracket.
data Elimination = Single | Double deriving (Show, Eq, Ord)

-- | The bracket location of a game.
--
-- For 'Duel' 'Single' or 'FFA', most matches exist in  the winners bracket ('WB')
-- , with the exception of the bronze final and possible crossover matches.
--
-- 'Duel' 'Double' or 'FFA' with crossovers will have extra matches in the loser bracket ('LB').
data Bracket = WB | LB deriving (Show, Eq, Ord)

-- | Players and Results zip to the correct association list.
-- 'scores' will obtain this ordered association list safely.
data Game = Game {
  players :: [Player]
, result  :: Maybe [Score]
} deriving (Show, Eq)

type Games = Map GameId Game

-- | 'score' clarification types.
type Position = Int
type Score = Int
type Player = Int
type Seed = Int

-- | Record of each player's accomplishments in the current tournament.
data Result = Result {
  -- | Player associated with the record.
  player    :: Int
  -- | Placement of the player associated with this record.
, placement :: Int
  -- | Number of games the player associated with this record won.
, wins      :: Int
  -- | Sum of scores for the games the associated player played.
, total     :: Int
} deriving (Show)

-- | Results in descending order of placement.
--
-- Only constructed by 'score' once the last game was played.
type Results = [Result]

type GroupSize = Int
type Advancers = Int

data Rules = FFA GroupSize Advancers | Duel Elimination
type Size = Int

data Tournament = Tourney {
  size      :: Size
, crossover :: Bool
, rules     :: Rules
, games     :: Games
, results   :: Maybe Results
}

-- Internal helpers
gameZip :: Game -> [(Player, Score)]
gameZip m = zip (players m) (fromJust (result m))
gameSort :: [(Player, Score)] -> [(Player, Score)]
gameSort = reverse . sortBy (comparing snd)

-- | Sorted player identifier list by scores.
--
-- If this is called on an unscored match a (finite) list zeroes is returned.
-- This is consistent with the internal representation of placeholders in Matches.
-- TODO: could also use gameZip?
scores :: Game -> [Player]
scores g@(Game pls msc)
  | Just _ <- msc = map fst . gameSort . gameZip $ g
  | otherwise = replicate (length pls) 0

-- | The first and last elements from scores.
winner, loser :: Game -> Player
winner = head . scores
loser = last . scores

-- Duel specific helper
pow :: Int -> Int
pow = ceiling . logBase 2 . fromIntegral

-- | Count the number of rounds in a given bracket in a Tournament.
count :: Tournament -> Bracket -> Int
count Tourney { rules = Duel Single, size = np } br = if br == WB then pow np else 0 -- 1 with bronze
count Tourney { rules = Duel Double, size = np } br = (if br == WB then 1 else 2) * pow np
count Tourney { rules = FFA _ _, games = ms } WB = round . fst . Map.findMax $ ms
count Tourney { rules = FFA _ _} LB = 0

-- Scoring and construction helper
woScores :: [Player] -> Maybe [Score]
woScores ps
  | 0 `notElem` ps && -1 `elem` ps = Just $ map (\x -> if x == -1 then 0 else 1) ps
  | otherwise = Nothing

-- | Create match shells for an FFA elimination tournament.
-- Result comes pre-filled in with either top advancers or advancers `intersect` seedList.
-- This means what the player numbers represent is only fixed per round.
-- TODO: Either String Tournament as return for intelligent error handling
tournament :: Rules -> Size -> Tournament
tournament rs@(FFA gs adv) np
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
        hideSeeds = map $ map $ const 0
        nextGroup g = hideSeeds . groups gs $ leftover where
          -- force zero non-eliminating matches unless only 1 left
          advm = max 1 $ adv - (gs - minsize g)
          leftover = length g * advm

        playoffs = takeWhile ((>1) . length) . iterate nextGroup . groups gs $ np
        final = nextGroup $ last playoffs
        grps = playoffs ++ [final]

        -- finally convert raw group lists to matches
        makeRound grp r = zipWith makeMatch grp [1..] where
          makeMatch g i = (GameId WB r i, Game g Nothing)

        ms = Map.fromList . concat $ zipWith makeRound grps [1..]
    in Tourney { size = np, rules = rs, games = ms, results = Nothing, crossover = False }


-- | Create match shells for an elimination tournament
-- hangles walkovers and leaves the tournament in a stable initial state
tournament rs@(Duel e) np
  -- Enforce minimum 4 players for a tournament. It is possible to extend to 2 and 3, but:
  -- 3p uses a 4p model with one WO => == RRobin in Double, == Unfair in Single
  -- 2p Single == 1 best of 1 match, 2p Double == 1 best of 3 match
  -- and grand final rules fail when LB final is R1 (p=1) as GF is then 2*p-1 == 1 ↯
  | np < 4 = error "Need >=4 competitors for an elimination tournament"
  | otherwise = Tourney { size = np, rules = rs, games = ms, results = Nothing, crossover = True} where
    p = pow np

    -- complete WBR1 by filling in -1 as WO markers for missing (np'-np) players
    markWO (x, y) = map (\a -> if a <= np then a else -1) [x,y]
    makeWbR1 i = (l, Game pl (woScores pl)) where
      l = GameId WB 1 i
      pl = markWO $ seeds p i

    -- make WBR2 and LBR1 shells by using the paired WBR1 results to propagate winners/WO markers
    propagateWbR1 br ((_, m1), (l2, m2)) = (l, Game pl (woScores pl)) where
      (l, pl)
        | br == WB = (GameId WB 2 g, map winner [m1, m2])
        | br == LB = (GameId LB 1 g, map loser [m1, m2])
      g = game l2 `div` 2

    -- make LBR2 shells by using LBR1 results to propagate WO markers if 2x
    makeLbR2 (l1, m1) = (l, Game pl Nothing) where
      l = GameId LB 2 $ game l1
      plw = winner m1
      pl = if odd (game l1) then [0, plw] else [plw, 0]

    -- construct (possibly) non-empty rounds
    wbr1 = map makeWbR1 [1..2^(p-1)]
    wbr1pairs = take (2^(p-2))
      $ filter (even . game . fst . snd) $ zip wbr1 (tail wbr1)
    wbr2 = map (propagateWbR1 WB) wbr1pairs
    lbr1 = map (propagateWbR1 LB) wbr1pairs
    lbr2 = map makeLbR2 lbr1

    -- construct (definitely) empty rounds
    wbRest = concatMap makeRound [3..p] where
      makeRound r = map (GameId WB r) [1..2^(p-r)]
      --bfm = MID LB (R 1) (G 1) -- bronze final here, exception

    lbRest = map gfms [2*p-1, 2*p] ++ concatMap makeRound [3..2*p-2] where
      makeRound r = map (GameId LB r) [1..(2^) $ p - 1 - (r+1) `div` 2]
      gfms r = GameId LB r 1

    toMap = Map.fromSet (const (Game [0,0] Nothing)) . Set.fromList

    -- finally, union the mappified brackets
    wb = Map.union (toMap wbRest) $ Map.fromList $ wbr1 ++ wbr2
    lb = Map.union (toMap lbRest) $ Map.fromList $ lbr1 ++ lbr2
    ms = if e == Single then wb else wb `Map.union` lb



makeResults :: Tournament -> Games -> Maybe Results
makeResults (Tourney {rules = Duel e, size = np}) ms
  | e == Single
  , Just wbf@(Game _ (Just _)) <- Map.lookup (GameId WB p 1) ms -- final played
  -- bf lookup here if included!
  = Just . scorify . winner $ wbf

  | e == Double
  , Just gf1@(Game _ (Just gf1sc)) <- Map.lookup (GameId LB (2*p-1) 1) ms -- gf1 played
  , Just gf2@(Game _ gf2sc) <- Map.lookup (GameId LB (2*p) 1) ms  -- gf2 maybe played
  , isJust gf2sc || maximum gf1sc == head gf1sc -- gf2 played || gf1 conclusive
  = Just . scorify . winner $ if isJust gf2sc then gf2 else gf1

  | otherwise = Nothing

  where
    p = pow np

    -- maps (last bracket's) maxround to the tie-placement
    toPlacement :: Elimination -> Int -> Int
    toPlacement Double maxlbr = if metric <= 4 then metric else 2^(k+1) + 1 + oddExtra where
      metric = 2*p + 1 - maxlbr
      r = metric - 4
      k = (r+1) `div` 2
      oddExtra = if odd r then 0 else 2^k
    toPlacement Single maxr = if metric <= 1 then metric else 2^r + 1 where
      metric = p+1 - maxr
      r = metric - 1

    -- scoring function assumes winner has been calculated so all that remains is:
    -- sort by maximum (last bracket's) round number descending, possibly flipping winners
    -- TODO: portions of this could possibly be used as a rules agnostic version
    scorify :: Int -> Results
    scorify w = map mergeLists placements where
      mergeLists (pl, pos) = Result { player = pl, placement = pos
        , wins = extract wins, total = extract scoreSum } where
        extract = fromMaybe 0 . lookup pl

      -- all pipelines start with this. 0 should not exist, -1 => winner got further
      -- scores not Just => should not have gotten this far by guard in score fn
      msnwo = Map.filter (all (>0) . players) ms

      wins = map (head &&& length)
        . group . sort
        . Map.foldr ((:) . winner) [] $ msnwo

      scoreSum = map (fst . head &&& foldr ((+) . snd) 0)
        . groupBy ((==) `on` fst)
        . sortBy (comparing fst)
        . Map.foldr ((++) . gameZip) [] $ msnwo

      placements = fixFirst
        . sortBy (comparing snd)
        . map (second (toPlacement e) . (fst . head &&& foldr (max . snd) 1))
        . groupBy ((==) `on` fst)
        . sortBy (comparing fst)
        . Map.foldrWithKey rfold [] $ msnwo

      rfold (GameId br r _) m acc =
        if (e == Single && br == WB) || (e == Double && br == LB)
          then (++ acc) . map (id &&& const r) $ players m
          else acc

      -- reorder start and make sure 2nd element has second place, as toPlacement cant distinguish
      fixFirst (x@(a,_):y@(b,_):rs) = if a == w then x : (b,2) : rs else y : (a,2) : rs
      fixFirst _ = error "<2 players in Match sent to flipFirst"
      -- TODO: if bronzeFinal then need to flip 3 and 4 possibly as well
      --fixForth (x:y:c:d:ls)

makeResults (Tourney {rules = FFA _ _, size = _}) ms
  | (_, f@(Game _ (Just _))) <- Map.findMax ms
  = Just scorify

  | otherwise = Nothing
  where
    scorify :: Results
    scorify = [Result 0 0 0 0]


-- | Score a match in a tournament and propagate winners/losers.
-- TODO: make a strict version of this
-- TODO: documentation absorb the individual functions?
-- TODO: test if MID exists, subfns throw if lookup fail
score :: GameId -> [Score] -> Tournament -> Tournament
score gid sc trn@(Tourney { rules = r, size = np, games = ms })
  | Duel e <- r
  , Just (Game pls _) <- Map.lookup gid ms
  , all (>0) pls
  = let msUpd = execState (scoreDuel (pow np) e gid sc pls) ms
        rsUpd = makeResults trn msUpd
    in trn { games = msUpd, results = rsUpd }

  | FFA s _ <- r
  , Just (Game pls _) <- Map.lookup gid ms
  , any (>0) pls
  = let msUpd = execState (scoreFFA s gid sc pls) ms
    in trn { games = msUpd }

  | otherwise = error "game not scorable"

scoreFFA :: GroupSize -> GameId -> [Score] -> [Player] -> State Games ()
scoreFFA gs gid@(GameId _ r _) scrs pls = do
  -- 1. score given game
  let m = Game pls $ Just scrs
  modify $ Map.adjust (const m) gid

  -- 2. if end of round, fill in next round
  currRnd <- gets $ Map.elems . Map.filterWithKey (const . (==r) . round)
  when (all (isJust . result) currRnd) $ do
    -- find the number of players we need in next round
    numNext <- gets $ Map.foldr ((+) . length . players) 0
                    . Map.filterWithKey (const . (==r+1) . round)
    -- recreate next round by using last round results as new seeding
    -- update next round by overwriting duplicate keys in next round
    modify $ flip (Map.unionWith (flip const)) $ makeRnd currRnd numNext
    return ()

  where
    -- make round (r+1) from the games in round r and the top n to take
    makeRnd :: [Game] -> Size -> Games
    makeRnd gms = Map.fromList . nextGames . grpMap (seedToPlayer gms) . groups gs

    seedToPlayer :: [Game] -> [(Seed, Player)]
    seedToPlayer = zip [1..] . map fst . gameSort . concatMap gameZip

    grpMap :: [(Seed, Player)] -> [[Seed]] -> [[Player]]
    grpMap assoc = map . map $ fromJust . flip lookup assoc

    nextGames :: [[Player]] -> [(GameId, Game)]
    nextGames = zipWith (\i g -> (GameId WB (r+1) i, Game g Nothing)) [1..]


-- | Update the scores of a duel in an elimination tournament.
-- Returns an updated tournament with the winner propagated to the next round,
-- and the loser propagated to the loser bracket if applicable.
scoreDuel :: Int -> Elimination -> GameId -> [Score] -> [Player] -> State Games (Maybe Game)
scoreDuel p e gid scrs pls = do
  -- 1. score given game
  let m = Game pls $ Just scrs
  modify $ Map.adjust (const m) gid

  -- 2. move winner right
  let nprog = mRight True p gid
  nres <- playerInsert nprog $ winner m

  -- 3. move loser to down if we were in winners
  let dprog = mDown p gid
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
    playerInsert :: Maybe (GameId, Position) -> Player -> State Games (Maybe Game)
    playerInsert Nothing _ = return Nothing
    playerInsert (Just (gid, idx)) x = do
      tmap <- get
      let (updated, tupd) = Map.updateLookupWithKey updFn gid tmap
      put tupd
      return updated
        where updFn _ (Game plsi _) = Just $ Game plsm (woScores plsm) where
                plsm = if idx == 0 then [x, last plsi] else [head plsi, x]

    -- given tourney power, progress results, and insert results, of previous
    -- if it was woScored in playerInsert, produce new (progress, winner) pair
    woCheck :: Player
            -> Maybe (GameId, Position)
            -> Maybe Game
            -> Maybe (Maybe (GameId, Position), Player)
    woCheck p (Just (gid, _)) (Just mg)
      | w <- winner mg, w > 0 = Just (mRight False p gid, w)
      | otherwise = Nothing
    woCheck _ _ _ = Nothing


    -- right progress fn: winner moves right to (GameId, Position)
    mRight :: Bool -> Int -> GameId -> Maybe (GameId, Position)
    mRight gf2Check p (GameId br r g)
      | r < 1 || g < 1 = error "bad GameId"
      -- Nothing if last Game. NB: WB ends 1 round faster depending on e
      | r >= 2*p || (br == WB && (r > p || (e == Single && r == p))) = Nothing
      | br == LB  = Just (GameId LB (r+1) ghalf, pos)   -- standard LB progression
      | r == 2*p-1 && br == LB && gf2Check && maximum scrs == head scrs = Nothing
      | r == p    = Just (GameId LB (2*p-1) ghalf, 0)   -- WB winner -> GF1 path
      | otherwise = Just (GameId WB (r+1) ghalf, pos)   -- standard WB progression
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

    -- down progress fn : loser moves down to (GameId, Position)
    mDown :: Int -> GameId -> Maybe (GameId, Position)
    mDown p (GameId br r g)
      | e == Single = Nothing
      -- or case for bf: | e == Single && r == p-1 = Just (MID LB (R 1) (G 1), if odd g then 0 else 1)
      | r == 2*p-1 = Just (GameId LB (2*p) 1, 1) -- GF(1) loser moves to the bottom
      | br == LB || r > p = Nothing
      | r == 1    = Just (GameId LB 1 ghalf, pos)     -- WBR1 -> r=1 g/2 (LBR1 only gets input from WB)
      | otherwise = Just (GameId LB ((r-1)*2) g, pos) -- WBRr -> 2x as late per round in WB
        where
          ghalf = (g+1) `div` 2
          -- drop on top >R2, and <=2 for odd g to match bracket movement
          pos = if r > 2 || odd g then 0 else 1


-- | temp stuff
testcase :: IO ()
testcase = let
  upd :: GameId -> [Score] -> State Tournament ()
  upd id sc = do
    t <- get
    put $ score id sc t
    return ()
  {-
  manipDouble :: State Tournament ()
  manipDouble = do
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
    upd (MID LB (R 5) (G 1)) [0,3] -- gf1
    upd (MID LB (R 6) (G 1)) [1,2]

    return ()
  -}
  manipSingle :: State Tournament ()
  manipSingle = do
    upd (GameId WB 1 2) [2,3]
    upd (GameId WB 1 3) [1,2]
    upd (GameId WB 1 4) [0,1]

    upd (GameId WB 2 1) [1,0]
    upd (GameId WB 2 2) [1,0]

    upd (GameId WB 3 1) [1,0]

    return ()

  --a <- testor $ execState manipDouble $ tournament (Duel Double) 5
  in testor $ execState manipSingle $ tournament (Duel Single) 7


testor :: Tournament -> IO ()
testor Tourney { games = ms, results = rs } = do
  mapM_ print $ Map.assocs ms
  maybe (print "no results") (mapM_ print) rs

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
