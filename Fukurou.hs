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
import Data.Word
import System.Random
import Text.Printf

import Base
import FastBoard

data Fukurou = Fukurou (MVar StdGen) PlayerSide (MVar Game)

evaluateForSente :: FastBoard -> Float
evaluateForSente state@(FastBoard pieces (SengoPair cpSente cpGote)) =
	sum (map valueOfPiece $ map snd piecesSente) + evaluateCaptures cpSente +
	negate ((sum $ map valueOfPiece $ map snd piecesGote) + evaluateCaptures cpGote)
	where
		(piecesSente, piecesGote) = partition ((== Sente) . fst) $ M.elems pieces

--		evaluateCaptures caps = fromIntegral $  sum [valueOfPiece pieceType * num | (pieceType, num) <- assocs caps]
		evaluateCaptures caps = fromIntegral $ sum $ map valueOfPiece caps
		
		valueOfPiece FU = 1
		valueOfPiece KY = 3
		valueOfPiece KE = 4
		valueOfPiece GI = 5
		valueOfPiece KI = 6
		valueOfPiece KA = 8
		valueOfPiece HI = 10
		valueOfPiece TO = 7
		valueOfPiece NY = 6
		valueOfPiece NK = 6
		valueOfPiece NG = 6
		valueOfPiece UM = 10
		valueOfPiece RY = 12
		valueOfPiece OU = 1000

evaluateFor :: PlayerSide -> FastBoard -> Float
evaluateFor Sente state = evaluateForSente state
evaluateFor Gote state = negate $ evaluateForSente state

-- | Create an AI player. Player may or may not be thinking in the background.
-- A Player cannot adjust to illegal proceedings of a game, or multiple games
-- simultaneously.
createFukurou :: PlayerSide -> IO Fukurou
createFukurou side = do
	random <- newStdGen >>= newMVar
	game <- newEmptyMVar
	return $ Fukurou random side game

-- | Notify a game after a play.
notifyPlay :: Fukurou -> Game -> IO ()
notifyPlay (Fukurou _ side mGame) game = do
	empty <- isEmptyMVar mGame
	if empty
		then putMVar mGame game
		else swapMVar mGame game >> return ()

-- | Ask fukurou to generate next play.
askPlay :: Fukurou -> IO Play
askPlay (Fukurou mRandomGen side mGame) = do
	game <- readMVar mGame
	case Base.legalMovesConsideringCheck side $ latestBoard game of
		[] -> return Resign
		_ -> do
			let ((score, play), state) = runState
				(searchBestPlay (-10000) 10000 maxDepth side (compressBoard $ latestBoard game))
				(SearchState {numberOfBoards = 0, scoreCache = M.empty})
			printf "#eval: %d\n" (numberOfBoards state)
			printf "#stored boards: %d\n" (M.size $ scoreCache state)
			putStrLn $ "Score: " ++ show score
			return play
	where
		maxDepth = 3

data SearchState = SearchState {
		numberOfBoards :: !Int,
		scoreCache :: !(M.Map (PlayerSide, FastBoard) (Float, Play))
	}


evaluateBoard :: PlayerSide -> FastBoard -> State SearchState Float
evaluateBoard side board = do
	modify $! \state -> state {numberOfBoards = numberOfBoards state + 1}
	return $! evaluateFor side board

-- | Select optimal play for current side. Returns the play and score.
searchBestPlay :: Float -> Float -> Int -> PlayerSide -> FastBoard -> State SearchState (Float, Play)
searchBestPlay !alpha !beta !depth !side !board
	|depth == 0 = do
		score <- evaluateBoard side board
		return $! score `seq` (score, error "Terminal Node")
	|null plays = do
		score <- evaluateBoard side board
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
				(scoreEnemy, _) <- searchBestPlay (-beta) (-currentBestScore) (depth - 1) (flipSide side) (FastBoard.updateBoard play board)
				let score = (-scoreEnemy)
				if score > currentBestScore
					then findBestPlay score play plays
					else findBestPlay currentBestScore currentBestPlay plays

		plays = FastBoard.legalMovesConsideringCheck side board
