module Base where
import qualified Data.Map as M
import Data.List

data Piece
	= FU | KY | KE | GI | KI | KA | HI | OU
	| TO | NY | NK | NG      | UM | RY
	deriving(Show, Read, Eq)

data PlayerSide = Sente | Gote

-- | Easy to use (not efficient) representation of a board.
data BoardState = BoardState (M.Map (Int, Int) (PlayerSide, Piece)) [Piece] [Piece]


-- | from, to, piece type (after movement)
data Play = Play (Int, Int) (Int, Int) Piece


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
