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
import Data.Bits
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic
import qualified Data.Hashable
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.STRef
import qualified Data.Strict.Tuple as ST
import Data.Word
import System.Random
import Text.Printf

import Base
import FastBoard

data Fukurou = Fukurou Weight (MVar StdGen) PlayerSide (MVar Game)

data Weight = Weight (Array Piece Float)

defaultWeight :: Weight
defaultWeight = Weight pieceWeight
	where
		pieceWeight = array (FU, OU) [
			(FU, 1),
			(KY, 3),
			(KE, 4),
			(GI, 5),
			(KI, 6),
			(KA, 8),
			(HI, 10),
			(TO, 7),
			(NY, 6),
			(NK, 6),
			(NG, 6),
			(UM, 10),
			(RY, 12),
			(OU, 1000)]

-- | Strict sum.
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

evaluateForSente :: Weight -> FastBoard -> Float
evaluateForSente (Weight pieceWeight) state@(FastBoard pieces (SengoPair cpSente cpGote)) =
	(sum' $ map valueOfSidedPiece $ M.elems pieces) +
	(evaluateCaptures cpSente - evaluateCaptures cpGote)
	where
		valueOfSidedPiece (Sente ST.:!: p) = valueOfPiece p
		valueOfSidedPiece (Gote ST.:!: p) = - valueOfPiece p

		valueOfPiece !p = pieceWeight ! p
		evaluateCaptures !caps = sum' $ map valueOfPiece caps

evaluateFor :: Weight -> PlayerSide -> FastBoard -> Float
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
			let ((score, play), (number', cacheSize')) = runST $ do
				number <- newSTRef 0
				cache <- Data.HashTable.ST.Basic.new -- newSTRef M.empty
				let state = SearchState {numberOfBoards = number, scoreCache = cache}
				result <- searchBestPlay state weight (-10000) 10000 maxDepth side $ compressBoard $ latestBoard game
				number' <- readSTRef number
				cacheSize' <- liftM length (H.toList cache)
				return (result, (number', cacheSize'))
			printf "#eval: %d\n" number'
			printf "#stored boards: %d\n" cacheSize'
			putStrLn $ "Score: " ++ show score
			return play
	where
		maxDepth = 4

data SearchState s = SearchState {
		numberOfBoards :: STRef s Int,
		scoreCache :: Data.HashTable.ST.Basic.HashTable s (PlayerSide, FastBoard) (Float, Play)
	}

instance Data.Hashable.Hashable PlayerSide where
	hashWithSalt salt Sente = salt
	hashWithSalt salt Gote = complement salt

modifySTRef' ref f= do
	x <- readSTRef ref
	let x' = f x
	x' `seq` writeSTRef ref x'

evaluateBoard :: SearchState s -> Weight -> PlayerSide -> FastBoard -> ST s Float
evaluateBoard !state !weight !side !board = do
	modifySTRef' (numberOfBoards state) (+1)
	return $! evaluateFor weight side board

-- | Select optimal play for current side. Returns the play and score.
searchBestPlay :: SearchState s -> Weight -> Float -> Float -> Int -> PlayerSide -> FastBoard -> ST s (Float, Play)
searchBestPlay !state !weight !alpha !beta !depth !side !board
	|depth == 0 = do
		score <- evaluateBoard state weight side board
		return $! score `seq` (score, error "Terminal Node")
	|null plays = do
		score <- evaluateBoard state weight side board
		return $! score `seq` (score, Resign)
	|otherwise = do
		maybeEntry <- H.lookup (scoreCache state) (side, board)
		case maybeEntry of
			Nothing -> do
				entry <- findBestPlay alpha (head plays) (tail plays)
				H.insert (scoreCache state) (side, board) entry
				return entry
			Just entry -> do
				return entry
	where
		findBestPlay !currentBestScore !currentBestPlay [] = return (currentBestScore, currentBestPlay)
		findBestPlay !currentBestScore !currentBestPlay (play:plays)
			|currentBestScore > beta = return (currentBestScore, currentBestPlay)
			|otherwise = do
				(scoreEnemy, _) <- searchBestPlay state weight (-beta) (-currentBestScore) (depth - 1) (flipSide side) (FastBoard.updateBoard play board)
				let score = (-scoreEnemy)
				if score > currentBestScore
					then findBestPlay score play plays
					else findBestPlay currentBestScore currentBestPlay plays

		plays = FastBoard.legalMovesConsideringCheck side board
