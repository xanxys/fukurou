-- | Shogi "engine" representing a single AI player.
-- playing ability is inherently limited by time and memory;
-- so it's exposed as IO monad.
-- You can consider to expose pseudo-pure interfaces such as
-- solveTsumeShogi :: TimeoutSecond -> Maybe [Play]
module Fukurou where
import Control.Concurrent.MVar
import Control.Monad
import System.Random

import Base

data Fukurou = Fukurou (MVar StdGen) PlayerSide (MVar Game)

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
	case legalMoves side $ latestBoard game of
		[] -> return Resign
		plays -> do
			gen <- takeMVar mRandomGen
			let (index, gen') = randomR (0, length plays - 1) gen
			putMVar mRandomGen gen'
			return $ plays !! index
