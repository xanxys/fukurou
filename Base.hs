-- | This module provides easy-to-check, simple, inefficient code
-- to represent Shogi rules.
-- You can use this for UI, or automated testing of sophisticated board
-- representation.
module Base where
import qualified Data.Map as M
import Data.List

data Piece
	= FU | KY | KE | GI | KI | KA | HI | OU
	| TO | NY | NK | NG      | UM | RY
	deriving(Show, Read, Eq)

data PlayerSide = Sente | Gote deriving(Show, Eq, Ord)

-- | A sente-gote pair of any symmetrical information.
data Sengo a = Sengo a a


flipSide :: PlayerSide -> PlayerSide
flipSide Sente = Gote
flipSide Gote = Sente

-- | Easy to use (not efficient) representation of a board.
data BoardState = BoardState (M.Map (Int, Int) (PlayerSide, Piece)) [Piece] [Piece]


-- | from, to, piece type (after movement)
data Play
	= Play (Int, Int) (Int, Int) Piece
	| Resign
	deriving(Eq)

-- | Record of a valid shogi game up to a certain point.
data Game = Game {
	senteName :: String,
	goteName :: String,
	plays :: [Play],
	latestBoard :: BoardState}

getTurn :: Game -> PlayerSide
getTurn game
	|even $ length $ plays game = Sente
	|otherwise = Gote

-- | Add a play if it's a legal play.
addPlay :: Play -> Game -> Maybe Game
addPlay play game = Just $ game {plays = play : plays game}

-- TODO: consider draw case.
-- TODO: implement check-mate
winningSide :: Game -> Maybe PlayerSide
winningSide game
	|not (null $ plays game) && head (plays game) == Resign = Just $ getTurn game
	|otherwise = Nothing

initialBoardState :: BoardState
initialBoardState = BoardState (M.fromList pairs) [] []
	where
		pairs = gotePairs ++ sentePairs
		gotePairs =
			[((j, 1), (Gote, FU)) | j <- [1..9]] ++
			[((8, 2), (Gote, HI)), ((2, 2), (Gote, KA))] ++
			[((j, 3), (Gote, piece)) | (j, piece) <- zip [1..9] nonFuRow]

		sentePairs =
			[((j, 7), (Sente, FU)) | j <- [1..9]] ++
			[((8, 8), (Sente, KA)), ((2, 8), (Sente, HI))] ++
			[((j, 9), (Sente, piece)) | (j, piece) <- zip [1..9] nonFuRow]

		nonFuRow = [KY, KE, GI, KI, OU, KI, GI, KE, KY]

