{-# LANGUAGE BangPatterns #-}
-- | Shogi "engine" representing a single AI player.
-- playing ability is inherently limited by time and memory;
-- so it's exposed as IO monad.
-- You can consider to expose pseudo-pure interfaces such as
-- solveTsumeShogi :: TimeoutSecond -> Maybe [Play]
module Fukurou where
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Bits
import qualified Data.Hashable
import qualified Data.HashTable.Class
import qualified Data.HashTable.ST.Basic
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.STRef
import qualified Data.Strict.Tuple as ST
import Data.Time.Clock
import Data.Word
import System.Random
import System.Timeout
import Text.Printf

import Base
import FastBoard

data Fukurou = Fukurou Weight (MVar StdGen) PlayerSide (MVar Game)

data Weight = Weight (Array Piece Int)

type Score = Int

defaultWeight :: Weight
defaultWeight = Weight pieceWeight
	where
		pieceWeight = array (FU, OU) [
			(FU, 100),
			(KY, 300),
			(KE, 400),
			(GI, 500),
			(KI, 600),
			(KA, 800),
			(HI, 1000),
			(TO, 700),
			(NY, 680),
			(NK, 660),
			(NG, 640),
			(UM, 1100),
			(RY, 1200),
			(OU, 5000)]

-- | Strict sum.
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

evaluateForSente :: Weight -> FastBoard -> Score
evaluateForSente (Weight pieceWeight) state@(FastBoard pieces (SengoPair cpSente cpGote)) =
	(sum' $ map valueOfSidedPiece $ M.elems pieces) +
	(evaluateCaptures cpSente - evaluateCaptures cpGote) +
	(length (FastBoard.movingPlays Sente state) - length (FastBoard.movingPlays Gote state)) * 25
	where
		valueOfSidedPiece (Sente ST.:!: p) = valueOfPiece p
		valueOfSidedPiece (Gote ST.:!: p) = - valueOfPiece p

		valueOfPiece !p = pieceWeight ! p
		evaluateCaptures !caps = sum' $ map (\(t, n) -> n * valueOfPiece t) $ M.assocs caps

evaluateFor :: Weight -> PlayerSide -> FastBoard -> Score
evaluateFor w Sente state = evaluateForSente w state
evaluateFor w Gote state = negate $ evaluateForSente w state

-- | Create an AI player. Player may or may not be thinking in the background.
-- A Player cannot adjust to illegal proceedings of a game, or multiple games
-- simultaneously.
createFukurou :: PlayerSide -> IO Fukurou
createFukurou side = do
	random <- newStdGen >>= newMVar
	game <- newEmptyMVar
	return $ Fukurou defaultWeight random side game

-- | Notify a game after a play.
notifyPlay :: Fukurou -> Game -> IO ()
notifyPlay (Fukurou _ _ side mGame) game = do
	empty <- isEmptyMVar mGame
	if empty
		then putMVar mGame game
		else swapMVar mGame game >> return ()

-- | Ask fukurou to generate next play.
askPlay :: Fukurou -> IO Play
askPlay (Fukurou weight mRandomGen side mGame) = do
	game <- readMVar mGame
	case Base.legalMovesConsideringCheck side $ latestBoard game of
		[] -> return Resign
		_ -> do
			deadline <- liftM (addUTCTime searchTimeout) getCurrentTime
			iterativeDFS [] game deadline (error "Initial search too deep") initialDepth
	where
		searchTimeout = 15
		initialDepth = 3

		-- First iteration must finish in time.
		iterativeDFS cache game deadline best depth = do
			delta <- liftM (diffUTCTime deadline) getCurrentTime
			let deltaMicrosec = floor $ realToFrac delta * 10^6
			result <- timeout deltaMicrosec (searchWithMaxDepth cache game depth)
			case result of
				Nothing -> return best
				Just (best', cache') -> iterativeDFS cache' game deadline best' (depth + 1)

		searchWithMaxDepth :: PureCache -> Game -> Int -> IO (Play, PureCache)
		searchWithMaxDepth !cache0 !game !depth = do
			gen <- takeMVar mRandomGen
			printf "== Searching with depth=%d\n" depth
			t0 <- getCurrentTime
			let ((score, play), (number', cache'), gen') = runST $ do
				number <- newSTRef 0
				cache <- Data.HashTable.Class.fromList cache0
				sRandGen <- newSTRef gen
				let state = SearchState {
					numberOfBoards = number,
					randGen = sRandGen,
					scoreCache = cache}
				result <- searchBestPlay state weight (-100000) 100000 True depth side $ compressBoard $ latestBoard game
				number' <- readSTRef number
				cache' <- Data.HashTable.Class.toList cache
				gen' <- readSTRef sRandGen
				return (result, (number', cache'), gen')
			putMVar mRandomGen gen'
			printf "* #eval: %d\n" number'
			printf "* #stored boards: %d\n" $ length cache'
			printf "* best play: %s\n" (show play)
			printf "* score: %d\n" score
			t1 <- getCurrentTime
			printf "* time: %fs\n" (realToFrac $ t1 `diffUTCTime` t0 :: Float)
			return (play, cache')

type PureCache = [((PlayerSide, FastBoard), (Int, (Score, Play)))]

data SearchState s = SearchState {
		numberOfBoards :: STRef s Int,
		randGen :: STRef s StdGen,
		scoreCache :: Data.HashTable.ST.Basic.HashTable s (PlayerSide, FastBoard) (Int, (Score, Play))
	}

instance Data.Hashable.Hashable PlayerSide where
	hashWithSalt salt Sente = salt
	hashWithSalt salt Gote = complement salt

modifySTRef' ref f= do
	x <- readSTRef ref
	let x' = f x
	x' `seq` writeSTRef ref x'

shuffleWith :: SearchState s -> [a] -> ST s [a]
shuffleWith !state !xs = do
	gen <- readSTRef $ randGen state
	let (xs', gen') = shuffle xs gen
	gen' `seq` writeSTRef (randGen state) gen'
	return xs'

evaluateBoard :: SearchState s -> Weight -> PlayerSide -> FastBoard -> ST s Score
evaluateBoard !state !weight !side !board = do
	modifySTRef' (numberOfBoards state) (+1)
	return $! evaluateFor weight side board

-- | Select optimal play for current side. Returns the play and score.
searchBestPlay :: SearchState s -> Weight -> Score -> Score ->
	Bool -> Int -> PlayerSide -> FastBoard -> ST s (Score, Play)
searchBestPlay !state !weight !alpha !beta !shufflePlays !depth !side !board
	|depth == 0 = do
		score <- evaluateBoard state weight side board
		return $! score `seq` (score, error "Terminal Node")
	|null plays = return (-10000, Resign)
	|otherwise = do
		maybeEntry <- lookupCache
		case maybeEntry of
			Nothing -> do
				--plays' <- if shufflePlays then shuffleWith state plays else return plays
				entry <- findBestPlay alpha (error "Dummy Play") plays
				Data.HashTable.Class.insert (scoreCache state) (side, board) (depth, entry)
				return entry
			Just entry -> return entry
	where
		lookupCache = do
			maybeEntry <- Data.HashTable.Class.lookup (scoreCache state) (side, board)
			case maybeEntry of
				Nothing -> return Nothing
				Just (cachedDepth, entry) -> do
					if cachedDepth < depth
						then return Nothing
						else return $! Just entry
		findBestPlay !bestScore bestPlay [] = return (bestScore, bestPlay)
		findBestPlay !bestScore bestPlay (play:plays)
			|bestScore >= beta = return (bestScore, bestPlay)
			|otherwise = do
				(scoreEnemy, _) <- searchBestPlay state weight
					(-beta) (-bestScore) False (depth - 1) (flipSide side) (FastBoard.updateBoard play board)
				let score = (-scoreEnemy)
				if score > bestScore
					then findBestPlay score play plays
					else findBestPlay bestScore bestPlay plays

		plays = filter (not . isGenerallyUselessPlay side) $! FastBoard.legalMovesConsideringCheck side board

-- | Check if any move exists that's always better than the given move.
-- (there's none if you consider 打ち歩詰め #1, but we ignore it)
isGenerallyUselessPlay :: PlayerSide -> Play -> Bool
isGenerallyUselessPlay _ (Put _ _ _) = False
isGenerallyUselessPlay !side (Move posFrom posTo pieceType)
	|promotable = elem pieceType [FU, KA, HI]
	|otherwise = False
	where
		promotable = inEnemyTerritory side posFrom || inEnemyTerritory side posTo

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle !xs !gen = runST $ do
	g <- newSTRef gen
	let randomRST lohi = do
		(a,s') <- liftM (randomR lohi) (readSTRef g)
		writeSTRef g s'
		return a
	ar <- newArray n xs
	xs' <- forM [1..n] $ \i -> do
		j <- randomRST (i,n)
		vi <- readArray ar i
		vj <- readArray ar j
		writeArray ar j vi
		return vj
	gen' <- readSTRef g
	return (xs',gen')
	where
		n = length xs
		newArray :: Int -> [a] -> ST s (STArray s Int a)
		newArray n xs =  newListArray (1,n) xs
