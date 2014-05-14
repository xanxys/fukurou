{-# LANGUAGE BangPatterns #-}
-- | Shogi "engine" representing a single AI player.
-- playing ability is inherently limited by time and memory;
-- so it's exposed as IO monad.
-- You can consider to expose pseudo-pure interfaces such as
-- solveTsumeShogi :: TimeoutSecond -> Maybe [Play]
module Fukurou where
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State.Strict
import Data.Array.Unboxed
import Data.Bits
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
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
			let ((score, play), state) = runState
				(searchBestPlay weight (-10000) 10000 maxDepth side (compressBoard $ latestBoard game))
				(SearchState {numberOfBoards = 0, scoreCache = M.empty})
			printf "#eval: %d\n" (numberOfBoards state)
			printf "#stored boards: %d\n" (M.size $ scoreCache state)
			putStrLn $ "Score: " ++ show score
			return play
	where
		maxDepth = 4

data SearchState = SearchState {
		numberOfBoards :: !Int,
		scoreCache :: !(M.Map (PlayerSide, FastBoard) (Float, Play))
	}


evaluateBoard :: Weight -> PlayerSide -> FastBoard -> State SearchState Float
evaluateBoard !weight !side !board = do
	modify $! \state -> state {numberOfBoards = numberOfBoards state + 1}
	return $! evaluateFor weight side board

-- | Select optimal play for current side. Returns the play and score.
searchBestPlay :: Weight -> Float -> Float -> Int -> PlayerSide -> FastBoard -> State SearchState (Float, Play)
searchBestPlay !weight !alpha !beta !depth !side !board
	|depth == 0 = do
		score <- evaluateBoard weight side board
		return $! score `seq` (score, error "Terminal Node")
	|null plays = do
		score <- evaluateBoard weight side board
		return $! score `seq` (score, Resign)
	|otherwise = do
		cache <- liftM scoreCache get
		case M.lookup (side, board) cache of
			Nothing -> do
				entry <- findBestPlay alpha (head plays) (tail plays)
				modify $! \state -> state {scoreCache = M.insert (side, board) entry $ scoreCache state}
				return entry
			Just entry -> do
				return entry				
	where
		findBestPlay !currentBestScore !currentBestPlay [] = return (currentBestScore, currentBestPlay)
		findBestPlay !currentBestScore !currentBestPlay (play:plays)
			|currentBestScore > beta = return (currentBestScore, currentBestPlay)
			|otherwise = do
				(scoreEnemy, _) <- searchBestPlay weight (-beta) (-currentBestScore) (depth - 1) (flipSide side) (FastBoard.updateBoard play board)
				let score = (-scoreEnemy)
				if score > currentBestScore
					then findBestPlay score play plays
					else findBestPlay currentBestScore currentBestPlay plays

		plays = FastBoard.legalMovesConsideringCheck side board
