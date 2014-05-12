-- | Shogi "engine" representing a single AI player.
-- playing ability is inherently limited by time and memory;
-- so it's exposed as IO monad.
-- You can consider to expose pseudo-pure interfaces such as
-- solveTsumeShogi :: TimeoutSecond -> Maybe [Play]
module Fukurou where
import Control.Concurrent.MVar
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Ord
import System.Random

import Base

data Fukurou = Fukurou (MVar StdGen) PlayerSide (MVar Game)


evaluateForSente :: BoardState -> Float
evaluateForSente state@(BoardState pieces (SengoPair cpSente cpGote)) = 
	(if isCheck Sente state then -500 else 0) +
	(if isCheck Gote state then 500 else 0) +
	sum (map valueOfPiece $ map snd piecesSente ++ cpSente) +
	negate (sum $ map valueOfPiece $ map snd piecesGote ++ cpGote)
	where
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
			let (score, play) = searchBestPlay 2 side (latestBoard game)
			putStrLn $ "Score: " ++ show score
			return play

-- | Select optimal play for current side. Returns the play and score.
searchBestPlay :: Int -> PlayerSide -> BoardState -> (Float, Play)
searchBestPlay 0 side board = (evaluateFor side board, error "Terminal Node")
searchBestPlay depth side board
	|null plays = (evaluateFor side board, Resign)
	|otherwise =
		maximumBy (comparing fst) $
			[(fst $ searchBestPlay (depth - 1) (flipSide side) (updateBoard play board), play) | play <- plays]
	where
		plays = legalMovesConsideringCheck side board
