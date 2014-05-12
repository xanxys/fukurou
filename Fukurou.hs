-- | Shogi "engine" representing a single AI player.
-- playing ability is inherently limited by time and memory;
-- so it's exposed as IO monad.
-- You can consider to expose pseudo-pure interfaces such as
-- solveTsumeShogi :: TimeoutSecond -> Maybe [Play]
module Fukurou where
import Base

data Fukurou = Fukurou

-- | Create an AI player. Player may or may not be thinking in the background.
-- A Player cannot adjust to illegal proceedings of a game, or multiple games
-- simultaneously.
createFukurou :: PlayerSide -> IO Fukurou
createFukurou _ = return Fukurou

-- | Notify a play by the opponent.
notifyPlay :: Fukurou -> Game -> IO ()
notifyPlay _ _ = return ()

-- | Ask fukurou to generate next play.
askPlay :: Fukurou -> IO Play
askPlay _ = return $ Play (3, 3) (5, 5) FU

