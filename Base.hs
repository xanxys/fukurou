-- | This module provides easy-to-check, simple, inefficient code
-- to represent Shogi rules.
-- You can use this for UI, or automated testing of sophisticated board
-- representation.
module Base where
import qualified Data.Map as M
import Data.Functor
import Data.List
import Data.Maybe

data Piece
	= FU | KY | KE | GI | KI | KA | HI | OU
	| TO | NY | NK | NG      | UM | RY
	deriving(Show, Read, Eq, Ord)

-- | Promote promotable pieces, do nothing otherwise.
promote :: Piece -> Piece
promote FU = TO
promote KY = NY
promote KE = NK
promote GI = NG
promote KA = UM
promote HI = RY
promote p = p

unpromote :: Piece -> Piece
unpromote TO = FU
unpromote NY = KY
unpromote NK = KE
unpromote NG = GI
unpromote UM = KA
unpromote RY = HI
unpromote p = p

data PlayerSide = Sente | Gote deriving(Show, Eq, Ord)

-- | A sente-gote pair of any symmetrical information.
-- instead of using tuples, use Sengo to avoid stupid mistakes.
data SengoPair a = SengoPair a a

instance Functor SengoPair where
	fmap f (SengoPair s g) = SengoPair (f s) (f g)

flattenPair :: SengoPair a -> [a]
flattenPair (SengoPair s g) = [s, g]

lookupPair :: SengoPair a -> PlayerSide -> a
lookupPair (SengoPair s _) Sente = s
lookupPair (SengoPair _ g) Gote = g

partiallyModifyPair :: PlayerSide -> (a -> a) -> SengoPair a -> SengoPair a
partiallyModifyPair Sente f (SengoPair s g) = SengoPair (f s) g
partiallyModifyPair Gote f (SengoPair s g) = SengoPair s (f g)

flipSide :: PlayerSide -> PlayerSide
flipSide Sente = Gote
flipSide Gote = Sente

-- | Easy to use (not efficient) representation of a board.
data BoardState = BoardState (M.Map (Int, Int) (PlayerSide, Piece)) (SengoPair [Piece])


-- | from, to, piece type (after movement)
data Play
	= Play (Int, Int) (Int, Int) Piece
	| Resign
	deriving(Eq, Ord)

-- | Record of a valid shogi game up to a certain point.
data Game = Game {
	playerNames :: SengoPair String,
	plays :: [Play],
	latestBoard :: BoardState}

getTurn :: Game -> PlayerSide
getTurn game
	|even $ length $ plays game = Sente
	|otherwise = Gote

-- | Add a play if it's a legal play.
addPlay :: Play -> Game -> Maybe Game
addPlay play game
	|isJust (winningSide game) = Nothing
	|elem play currentLegalMoves = Just $ game {
		latestBoard = updateBoard play (latestBoard game),
		plays = play : plays game}
	|otherwise = Nothing  -- illegal move
	where
		currentLegalMoves = Resign : legalMoves (getTurn game) (latestBoard game)

-- | Apply a known-to-be-legal move to given `BoardState`.
updateBoard :: Play -> BoardState -> BoardState
updateBoard (Play posFrom posTo pieceTypeTo) (BoardState pieces captures)
	= case M.lookup posTo pieces of
		Nothing -> BoardState pieces' captures  -- no capture
		Just (_, pieceTypeToBeCaptured) -> BoardState pieces' (captures' pieceTypeToBeCaptured)
	where
		pieces' = M.insert posTo (side, pieceTypeTo) $ M.delete posFrom pieces
		captures' newPieceType = partiallyModifyPair side (newPieceType:) captures

		(side, pieceTypeFrom) = pieces M.! posFrom

-- TODO: consider draw case.
-- TODO: implement check-mate
winningSide :: Game -> Maybe PlayerSide
winningSide game
	|not (null $ plays game) && head (plays game) == Resign = Just $ getTurn game
	|otherwise = Nothing

initialBoardState :: BoardState
initialBoardState = BoardState (M.fromList pairs) (SengoPair [] [])
	where
		pairs = gotePairs ++ sentePairs
		gotePairs =
			[((j, 1), (Gote, piece)) | (j, piece) <- zip [1..9] nonFuRow] ++
			[((8, 2), (Gote, HI)), ((2, 2), (Gote, KA))] ++
			[((j, 3), (Gote, FU)) | j <- [1..9]]
		sentePairs =
			[((j, 7), (Sente, FU)) | j <- [1..9]] ++
			[((8, 8), (Sente, KA)), ((2, 8), (Sente, HI))] ++
			[((j, 9), (Sente, piece)) | (j, piece) <- zip [1..9] nonFuRow]

		nonFuRow = [KY, KE, GI, KI, OU, KI, GI, KE, KY]

-- | Legal moves: movings or puttings of pieces, excluding Resign.
-- TODO: implement putting moves
legalMoves :: PlayerSide -> BoardState -> [Play]
legalMoves side board = movingPlays side board ++ puttingMoves
	where
		puttingMoves = []

movingPlays :: PlayerSide -> BoardState -> [Play]
movingPlays side board@(BoardState pieces _) = concatMap generatePlaysFor friendPieces
	where
		generatePlaysFor (posFrom, (_, pieceType)) =
			concatMap playsAt $ destinations board side posFrom
			where
				playsAt posTo
					|promotable posFrom posTo pieceType = [Play posFrom posTo pieceType, Play posFrom posTo $ promote pieceType]
					|otherwise = [Play posFrom posTo pieceType]

		promotable posFrom posTo pieceType =
			(promote pieceType /= pieceType) &&
			(inEnemyTerritory side posFrom || inEnemyTerritory side posTo)

		friendPieces = filter ((==side) . fst . snd) $ M.assocs pieces

-- | Check if given position is enemy's or not.
inEnemyTerritory :: PlayerSide -> (Int, Int) -> Bool
inEnemyTerritory Sente (x, y) = y <= 3
inEnemyTerritory Gote (x, y) = y >= 7


-- | Movable positions considering other pieces but without mates.
destinations :: BoardState -> PlayerSide -> (Int, Int) -> [(Int, Int)]
destinations (BoardState pieces _) side pos = concatMap filterRun runs
	where
		runs = potentialDestinationsInfinite side pos piece

		filterRun (p:ps)
			|not (inBoard p) = []
			|otherwise = case M.lookup p pieces of
				Nothing -> p:filterRun ps
				Just (blockerSide, blockerPos) ->
					if blockerSide == side
						then []  -- cannot move into friend's place
						else [p]  -- can take enemy piece, but move past it
		filterRun [] = []

		(_, piece) = pieces M.! pos
		inBoard (x, y) = 1 <= x && x <= 9 && 1 <= y && y <= 9

-- | Movable positions without considering any other pieces.
potentialDestinationsInfinite :: PlayerSide -> (Int, Int) -> Piece -> [[(Int, Int)]]
potentialDestinationsInfinite side pos piece
	|side == Sente = potentialSenteDestinationsInfinite pos piece
	|side == Gote = map (map flipY) $ potentialSenteDestinationsInfinite (flipY pos) piece
	where
		flipY (x, y) = (x, 10 - y)

-- | Movable positions without considering any other pieces nor board boundary.
-- Returns "runs", each of which is blocked by a piece.
potentialSenteDestinationsInfinite :: (Int, Int) -> Piece -> [[(Int, Int)]]
potentialSenteDestinationsInfinite (x, y) FU = [[(x, y - 1)]]
potentialSenteDestinationsInfinite (x, y) KY = [[(x, y - i) | i <- [1..]]]
potentialSenteDestinationsInfinite (x, y) KE = makeIndependent [(x - 1, y - 2), (x + 1, y - 2)]
potentialSenteDestinationsInfinite (x, y) GI = makeIndependent [
	(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
	(x - 1, y + 1), (x + 1, y + 1)]
potentialSenteDestinationsInfinite (x, y) KI = makeIndependent [
	(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
	(x - 1, y), (x + 1, y),
	(x, y + 1)]
potentialSenteDestinationsInfinite (x, y) KA = [
	[(x - i, y - i) | i <- [1..]],
	[(x - i, y + i) | i <- [1..]],
	[(x + i, y - i) | i <- [1..]],
	[(x + i, y + i) | i <- [1..]]]
potentialSenteDestinationsInfinite (x, y) HI = [
	[(x - i, y) | i <- [1..]],
	[(x - i, y) | i <- [1..]],
	[(x, y - i) | i <- [1..]],
	[(x, y + i) | i <- [1..]]]
potentialSenteDestinationsInfinite (x, y) OU = makeIndependent [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

potentialSenteDestinationsInfinite pos TO = potentialSenteDestinationsInfinite pos KI
potentialSenteDestinationsInfinite pos NY = potentialSenteDestinationsInfinite pos KI
potentialSenteDestinationsInfinite pos NK = potentialSenteDestinationsInfinite pos KI
potentialSenteDestinationsInfinite pos NG = potentialSenteDestinationsInfinite pos KI
potentialSenteDestinationsInfinite pos@(x, y) UM =
	makeIndependent [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] ++
	potentialSenteDestinationsInfinite pos KA
potentialSenteDestinationsInfinite pos@(x, y) RY =
	makeIndependent [(x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)] ++
	potentialSenteDestinationsInfinite pos HI

makeIndependent = map (:[])
