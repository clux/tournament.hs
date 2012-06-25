{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Game.Tournament
-- Copyright   :  (c) Eirik Albrigtsen 2012
-- License     :  MIT
-- Maintainer  :  Eirik <clux> Albrigtsen
-- Stability   :  unstable
--
-- Tournament construction and maintenance including competition based structures and helpers.
--
-- This library is intended to be imported qualified as it exports functions that clash with
-- Prelude.
--
-- > import Game.Tournament as T
--
-- The Tournament structure contain a Map of 'GameId' -> 'Game' for its internal
-- representation and the 'GameId' keys are the location in the Tournament.
--
-- Duel tournaments are based on the theory from <http://clux.org/entries/view/2407>.
-- By using the seeding definitions listed there, there is almost only one way to
-- generate a tournament, and the ambivalence appears only in Double elimination.
--
-- We have additionally chosen that brackets should converge by having the losers bracket move upwards.
-- This is not necessary, but improves the visual layout when presented in a standard way.
--
-- FFA tournaments use a collection of sensible assumptions on how to
-- optimally split n people into s groups while minimizing the sum of seeds difference
-- between groups for fairness. At the end of each round, groups are recalculated from the scores
-- of the winners, and new groups are created for the next round.

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
   , Results -- type synonym
   , results

   , Result  -- no constructor, but accessors:
   , player
   , placement
   , wins
   , total

   , Size

   , Tournament -- no constructor
   , Score

   , GroupSize
   , Advancers

   --, Game(..)
   --, Player

   --, Games

   -- * Tournament Interface
   , tournament
   , score
   , count
   , scorable
   , keys

   -- -* Match Inspection
   --, scores
   --, winner
   --, loser

   , testcase
) where

import Prelude hiding (round)
import Numeric (showIntAtBase, readInt)
import Data.Char (intToDigit, digitToInt)
import Data.List (sort, sortBy, group, groupBy, genericTake, zipWith4)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Bits (shiftL)
import Data.Maybe (fromJust, isJust, fromMaybe)
import qualified Data.Map.Lazy as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Control.Arrow ((&&&), second)
import Control.Monad (when)
import Control.Monad.State (State, get, put, modify, execState, gets)
--import System.IO.Unsafe (unsafePerformIO) -- while developing

-- -----------------------------------------------------------------------------
-- TODO should somehow ensure 0 < i <= 2^(p-1) in the next fn

-- | Power of a tournament.
-- It's defined as 2^num_players rounded up to nearest power of 2.
--type Power = Int

--type GameNumber = Int
-- TODO: use int synonyms more liberally?


-- | Computes both the upper and lower player seeds for a duel elimiation match.
-- The first argument is the power of the tournament:
--
-- p :: 2^num_players rounding up to nearest power of 2
--
-- The second parameter is the game number i (in round one).
--
-- The pair (p,i) must obey
--
-- >p > 0 && 0 < i <= 2^(p-1).
seeds :: Int -> Int -> (Int, Int)
seeds p i
  | p > 0, i > 0, i <= 2^(p-1) = (1 - lastSeed + 2^p, lastSeed)
  | otherwise = error "seeds called outside well defined power game region"
  where
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
duelExpected p (a, b) = odd a && even b && a + b == 1 + 2^p

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
-- TODO: rename to length once it has been less awkwardly moved into an internal part
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

-- | Get the list of all GameIds in a Tournament.
-- This list is also ordered by GameId's Ord, and in fact,
-- if the corresponding games were scored in this order, the tournament would finish,
-- and scorable would only return False for a few special walkover games.
-- TODO: if introducing crossovers, this would not be true for LB crossovers
-- => need to place them in WB in an 'interim round'
keys :: Tournament -> [GameId]
keys = Map.keys . games

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


-- | Helper to create the tie-correct Player -> Position association list.
-- Requires a Round -> Position function to do the heavy lifting where possible,
-- the final Game and Maybe bronzefinal to not take out
-- the list of games prefiltered away non-final bracket and final games.
-- result zips with Player == [1..]
placementSort :: Game -> Maybe Game -> (Int -> Position) -> Games -> [Position]
placementSort fg bf toPlacement = map snd . sortBy (comparing fst)
  . prependTop 1 (Just fg)
  . prependTop (((+1) . length . players) fg) bf
  . excludeTop
  . map (second toPlacement . (fst . head &&& foldr (max . snd) 1))
  . groupBy ((==) `on` fst)
  . sortBy (comparing fst)
  . Map.foldrWithKey rfold []
  where
    pls = if isJust bf then concatMap players [fg, fromJust bf] else players fg
    rfold (GameId _ r _) m acc = (++ acc) . map (id &&& const r) $ players m

    prependTop :: Int -> Maybe Game -> [(Position, Player)] -> [(Position, Player)]
    prependTop strt g
      | isJust g = (++) . flip zip [strt..] . map fst . gameSort . gameZip . fromJust $ g
      | otherwise = id
    excludeTop :: [(Position, Player)] -> [(Position, Player)]
    excludeTop = filter ((`notElem` pls) . fst)

-- zips with Player == [1..]
sumScores :: Games -> [Score]
sumScores = map (foldr ((+) . snd) 0)
  . groupBy ((==) `on` fst)
  . sortBy (comparing fst)
  . Map.foldr ((++) . gameZip) []

-- zips with Player == [1..]
getWins :: Int -> Games -> [Int]
getWins np = map (subtract 1 . length) -- started out with one of each so we can count zeroes
  . group . sort
  . Map.foldr ((:) . winner) [1..np]

zipResults :: [Int] -> [Int] -> [Int] -> [Result]
zipResults a b = sortBy (comparing placement) . zipWith4 Result [1..] a b

makeResults :: Tournament -> Games -> Maybe Results
makeResults (Tourney {rules = Duel e, size = np}) ms
  | e == Single
  , Just wbf@(Game _ (Just _)) <- Map.lookup (GameId WB p 1) ms -- final played
  -- bf lookup here if included!
  = Just . scorify $ wbf

  | e == Double
  , Just gf1@(Game _ (Just gf1sc)) <- Map.lookup (GameId LB (2*p-1) 1) ms -- gf1 played
  , Just gf2@(Game _ gf2sc) <- Map.lookup (GameId LB (2*p) 1) ms  -- gf2 maybe played
  , isJust gf2sc || maximum gf1sc == head gf1sc -- gf2 played || gf1 conclusive
  = Just . scorify $ if isJust gf2sc then gf2 else gf1

  | otherwise = Nothing

  where
    p = pow np
    maxRnd = if e == Single then p else 2*p-1

    -- maps (last bracket's) maxround to the tie-placement
    toPlacement :: Elimination -> Int -> Position
    toPlacement Double maxlbr = if metric <= 4 then metric else 2^(k+1) + 1 + oddExtra where
      metric = 2*p + 1 - maxlbr
      r = metric - 4
      k = (r+1) `div` 2
      oddExtra = if odd r then 0 else 2^k
    toPlacement Single maxr = if metric <= 1 then metric else 2^r + 1 where
      metric = p+1 - maxr
      r = metric - 1

    scorify :: Game -> Results
    scorify f = zipResults placements (getWins np ms) (sumScores msnwo) where
      -- all pipelines start with this. 0 should not exist, -1 => winner got further
      -- scores not Just => should not have gotten this far by guard in score fn
      msnwo = Map.filter (all (>0) . players) ms

      placements = placementSort f Nothing (toPlacement e)
        . Map.filterWithKey lastBracketNotFinal $ msnwo

      lastBracketNotFinal k _ = round k < maxRnd && lastBracket (bracket k)
      lastBracket br = (e == Single && br == WB) || (e == Double && br == LB)




makeResults (Tourney {rules = FFA _ _, size = np}) ms
  | (GameId _ maxRnd _, f@(Game _ (Just _))) <- Map.findMax ms
  = Just $ scorify maxRnd f

  | otherwise = Nothing
  where
    -- rsizes :: [(RoundNr, NumPlayers)] lookup helper for toPlacement
    rsizes = map (fst . head &&& foldr ((+) . snd) 0)
      . groupBy ((==) `on` fst)
      . sortBy (comparing fst)
      . Map.foldrWithKey rsizerf [] $ ms
      where
        rsizerf gid g acc = (round gid, (length . players) g) : acc

    -- maps a player's maxround to the tie-placement (called for r < maxRnd)
    -- simplistic :: 1 + number of people who got through to next round
    toPlacement :: Int -> Position
    toPlacement maxrp = (1+) . fromJust . lookup (maxrp + 1) $ rsizes

    scorify :: Int -> Game -> Results
    scorify maxRnd f = zipResults placements (getWins np ms) (sumScores ms) where
      -- NB: WO markers or placeholders should NOT exist when scorify called!

      -- placements using common helper, having prefiltered final game(round)
      placements = placementSort f Nothing toPlacement
        . Map.filterWithKey (\k _ -> round k < maxRnd) $ ms


playersReady :: GameId -> Tournament -> Maybe [Player]
playersReady gid t
  | Just (Game pls _) <- Map.lookup gid $ games t
  , all (>0) pls
  = Just pls
  | otherwise = Nothing

-- | Check if a GameId exists and is ready to be scored through 'score'.
scorable :: GameId -> Tournament -> Bool
scorable gid = isJust . playersReady gid

-- | Checks if a GameId is 'scorable' and it will not propagate to an already scored Game.
-- Guarding Tournament updates with this ensures it is never in an inconsistent state.
-- TODO: really needs access to mRight, mDown (if duel) to ensure they exist
-- TODO: if FFA only allow scoring if NO matches in the next round have been scored
safeScorable :: GameId -> Tournament -> Bool
safeScorable = undefined

-- | Score a match in a tournament and propagate winners/losers.
-- If match is not 'scorable', the Tournament will pass through unchanged.
--
-- For a Duel tournament, winners (and losers if Double) are propagated immediately,
-- wheras FFA tournaments calculate winners at the end of the round (when all games played).
--
-- There is no limitation on re-scoring old games, so care must be taken to not update too far
-- back ones and leaving the tournament in an inconsistent state. When scoring games more than one
-- round behind the corresponding active round, the locations to which these propagate must
-- be updated manually.
--
-- To prevent yourself from never scoring older matches, only score games for which
-- 'safeScorable' returns True. Though this has not been implemented yet.
--
-- > gid = (GameId WB 2 1)
-- > tUpdated = if safeScorable gid then score gid [1,0] t else t
--
-- TODO: strictify this function
-- TODO: better to do a scoreSafe? // call this scoreUnsafe
score :: GameId -> [Score] -> Tournament -> Tournament
score gid sc trn@(Tourney { rules = r, size = np, games = ms })
  | Duel e <- r
  , Just pls <- playersReady gid trn
  , length sc == 2
  = let msUpd = execState (scoreDuel (pow np) e gid sc pls) ms
        rsUpd = makeResults trn msUpd
    in trn { games = msUpd, results = rsUpd }

  | FFA s adv <- r
  , Just pls <- playersReady gid trn
  , length sc == length pls
  = let msUpd = execState (scoreFFA s adv gid sc pls) ms
        rsUpd = makeResults trn msUpd
    in trn { games = msUpd, results = rsUpd }

  -- somewhat less ideally, if length sc /= length pls this now also fails silently even if socable passes
  | otherwise = trn


scoreFFA :: GroupSize -> Advancers -> GameId -> [Score] -> [Player] -> State Games ()
scoreFFA gs adv gid@(GameId _ r _) scrs pls = do
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
    makeRnd gms = Map.fromList . nextGames . grpMap (seedAssoc False gms) . groups gs

    -- This sorts all players by overall scores (to help pick best crossover candidates)
    -- Or, if !takeAll, sort normally by only including the advancers from each game.
    seedAssoc :: Bool -> [Game] -> [(Seed, Player)]
    seedAssoc takeAll rnd
      | takeAll   = seedify . concatMap gameZip $ rnd
      | otherwise = seedify . concatMap (take (rndAdv rnd) . gameSort . gameZip) $ rnd
        where
          -- Find out how many to keep from each round before sorting overall
          rndAdv :: [Game] -> Advancers
          rndAdv = max 1 . (adv - gs +) . minimum . map (length . players)

          seedify :: [(Player, Score)] -> [(Seed, Player)]
          seedify = zip [1..] . map fst . gameSort

    grpMap :: [(Seed, Player)] -> [[Seed]] -> [[Player]]
    grpMap assoc = map . map $ fromJust . flip lookup assoc

    nextGames :: [[Player]] -> [(GameId, Game)]
    nextGames = zipWith (\i g -> (GameId WB (r+1) i, Game g Nothing)) [1..]

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

  return Nothing
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
          ghalf = if br == LB && odd r then g else (g+1) `div` 2
          pos
            | br == WB = if odd g then 0 else 1         -- WB maintains standard alignment
            | r == 2*p-2 = 1                            -- LB final winner => bottom of GF
            | r == 2*p-1 = 0                            -- GF(1) winnner moves to the top [semantic]
            | r > 1 && odd r = 1                        -- winner usually takes the bottom position
            | r == 1 = if odd g then 1 else 0           -- first rounds sometimes goto bottom
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


-- testing stuff
upd :: [Score] -> GameId -> State Tournament ()
upd sc id = do
  t <- get
  put $ score id sc t
  return ()

manipDuel :: [GameId] -> State Tournament ()
manipDuel = mapM_ (upd [1,0])

manipFFA :: State Tournament ()
manipFFA = do
  upd [1,2,3,4] $ GameId WB 1 1
  upd [5,3,2,1] $ GameId WB 1 2
  upd [2,4,2,1] $ GameId WB 1 3
  upd [6,3,2,1] $ GameId WB 1 4

  upd [1,2,3,4] $ GameId WB 2 1

testor :: Tournament -> IO ()
testor Tourney { games = ms, results = rs } = do
  mapM_ print $ Map.assocs ms
  maybe (print "no results") (mapM_ print) rs

testcase :: IO ()
testcase = do
  --let t = tournament (Duel Double) 8
  --testor $ execState (manipDuel (keys t)) t

  let t = tournament (FFA 4 1) 16
  testor $ execState manipFFA t


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
