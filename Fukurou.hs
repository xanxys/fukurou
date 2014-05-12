-- | Shogi "engine" representing a single AI player.
-- playing ability is inherently limited by time and memory;
-- so it's exposed as IO monad.
-- You can consider to expose pseudo-pure interfaces such as
-- solveTsumeShogi :: TimeoutSecond -> Maybe [Play]
module Fukurou where
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as M
import Data.Ord
import System.Random
import Text.Printf

import Base

data Fukurou = Fukurou (MVar StdGen) PlayerSide (MVar Game)


evaluateForSente :: BoardState -> Float
evaluateForSente state@(BoardState pieces (SengoPair cpSente cpGote)) =
	(if null plays then -1000 else 0) +
	sum (map valueOfPiece $ map snd piecesSente ++ cpSente) +
	negate (sum $ map valueOfPiece $ map snd piecesGote ++ cpGote)
	where
		plays = legalMovesConsideringCheck Sente state
		(piecesSente, piecesGote) = partition ((== Sente) . fst) $ M.elems pieces
		
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
		valueOfPiece OU = 0

evaluateFor :: PlayerSide -> BoardState -> Float
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
	case legalMovesConsideringCheck side $ latestBoard game of
		[] -> return Resign
		plays -> do
			let ((score, play), state) = runState
				(searchBestPlay (-10000) 10000 4 side (latestBoard game))
				(SearchState {numberOfBoards = 0, scoreCacheSente = M.empty})
			printf "#boards: %d\n" (numberOfBoards state)
			printf "#unique boards: %d\n" (M.size $ scoreCacheSente state)
			putStrLn $ "Score: " ++ show score
			return play

data SearchState = SearchState {
		numberOfBoards :: Int,
		scoreCacheSente :: M.Map BoardState Float
	}


evaluateBoard :: PlayerSide -> BoardState -> State SearchState Float
evaluateBoard side board = do
	modify $ \state -> state {numberOfBoards = numberOfBoards state + 1}
	return $! evaluateFor side board
	{-
	cache <- liftM scoreCacheSente get
	case M.lookup board cache of
		Nothing -> do
			let score = evaluateFor side board
			modify $ \state -> state {scoreCacheSente = M.insert board score cache}
			return score
		Just score -> do
			return score
	-}

-- | Select optimal play for current side. Returns the play and score.
searchBestPlay :: Float -> Float -> Int -> PlayerSide -> BoardState -> State SearchState (Float, Play)
searchBestPlay _ _ 0 side board = do
	score <- evaluateBoard side board
	return (score, error "Terminal Node")
searchBestPlay alpha beta depth side board
	|null plays = do
		score <- evaluateBoard side board
		return (score, Resign)
	|otherwise = findBestPlay alpha undefined plays
	where
		findBestPlay currentBestScore currentBestPlay [] = return (currentBestScore, currentBestPlay)
		findBestPlay currentBestScore currentBestPlay (play:plays)
			|currentBestScore > beta = return (currentBestScore, currentBestPlay)
			|otherwise = do
				(scoreEnemy, _) <- searchBestPlay (-beta) (-currentBestScore) (depth - 1) (flipSide side) (updateBoard play board)
				let score = (-scoreEnemy)
				if score > currentBestScore
					then findBestPlay score play plays
					else findBestPlay currentBestScore currentBestPlay plays

		plays = legalMovesConsideringCheck side board
