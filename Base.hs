-- | This module provides easy-to-check, simple, inefficient code
-- to represent Shogi rules.
-- You can use this for UI, or automated testing of sophisticated board
-- representation.
module Base where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Functor
import Data.List
import Data.Maybe

data Piece
	= FU | KY | KE | GI | KI | KA | HI
	| TO | NY | NK | NG      | UM | RY
	| OU
	deriving(Show, Read, Eq, Ord)

data ValidPosition = ValidPosition !Int !Int deriving(Show, Eq, Ord)

makePosition :: (Int, Int) -> ValidPosition
makePosition (x, y)
	|1 <= x && x <= 9 && 1 <= y && y <= 9 = ValidPosition x y
	|otherwise = error "Trying to create ValidPosition from an invalid position"


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
	deriving(Eq, Ord, Show)


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
data BoardState = BoardState (M.Map ValidPosition (PlayerSide, Piece)) (SengoPair [Piece])
	deriving(Eq, Ord, Show)

-- | from, to, piece type (after movement)
data Play
	= Move !ValidPosition !ValidPosition !Piece
	| Put !PlayerSide !ValidPosition !Piece
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
		currentLegalMoves = Resign : legalMovesConsideringCheck (getTurn game) (latestBoard game)

subtractCapture :: Piece -> [Piece] -> [Piece]
subtractCapture pieceToRemove (piece:pieces)
	|piece == pieceToRemove = pieces
	|piece /= pieceToRemove = piece : subtractCapture pieceToRemove pieces

-- | Apply a known-to-be-legal move to given `BoardState`.
updateBoard :: Play -> BoardState -> BoardState
updateBoard (Move posFrom posTo pieceTypeTo) (BoardState pieces captures)
	= case M.lookup posTo pieces of
		Nothing -> BoardState pieces' captures  -- no capture
		Just (_, pieceTypeToBeCaptured) -> BoardState pieces' (captures' $ unpromote pieceTypeToBeCaptured)
	where
		pieces' = M.insert posTo (side, pieceTypeTo) $ M.delete posFrom pieces
		captures' newPieceType = partiallyModifyPair side (newPieceType:) captures

		(side, pieceTypeFrom) = pieces M.! posFrom
updateBoard (Put side posTo pieceTypeTo) (BoardState pieces captures)
	= BoardState pieces' captures'
	where
		pieces' = M.insert posTo (side, pieceTypeTo) pieces
		captures' = partiallyModifyPair side (subtractCapture pieceTypeTo) captures

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
			[(makePosition (j, 1), (Gote, piece)) | (j, piece) <- zip [1..9] nonFuRow] ++
			[(makePosition (8, 2), (Gote, HI)), (makePosition (2, 2), (Gote, KA))] ++
			[(makePosition (j, 3), (Gote, FU)) | j <- [1..9]]
		sentePairs =
			[(makePosition (j, 7), (Sente, FU)) | j <- [1..9]] ++
			[(makePosition (8, 8), (Sente, KA)), (makePosition (2, 8), (Sente, HI))] ++
			[(makePosition (j, 9), (Sente, piece)) | (j, piece) <- zip [1..9] nonFuRow]

		nonFuRow = [KY, KE, GI, KI, OU, KI, GI, KE, KY]

isCheck :: PlayerSide -> BoardState -> Bool
isCheck side state@(BoardState pieces _) = any takesKing $ enemyMoves
	where
		-- Putting plays cannot capture king, moving plays is enough.
		enemyMoves = movingPlays (flipSide side) state
		takesKing (Move _ dst _) = dst == kingPos
		takesKing (Put _ _ _) = False
		[(kingPos, _)] = filter ((== (side, OU)) . snd) $ M.assocs pieces

legalMovesConsideringCheck :: PlayerSide -> BoardState -> [Play]
legalMovesConsideringCheck side board = filter (not . leadsToCheck) $ legalMoves side board
	where
		leadsToCheck play = isCheck side (updateBoard play board)

-- | Legal moves: movings or puttings of pieces, excluding Resign.
legalMoves :: PlayerSide -> BoardState -> [Play]
legalMoves side board = movingPlays side board ++ puttingMoves side board

puttingMoves :: PlayerSide -> BoardState -> [Play]
puttingMoves side (BoardState pieces captures) =
	filter (not . doubleFu) $ 
		[Put side posTo pieceType | posTo <- S.toList puttablePositions, pieceType <- puttableTypes]
	where
		doubleFu (Put side (ValidPosition x y) FU) =
			any (\posSearch -> maybe False (== (side, FU)) $ M.lookup posSearch pieces)
			[ValidPosition x ySearch | ySearch <- [1..9]]
		doubleFu _ = False

		puttablePositions = allPositions `S.difference` (M.keysSet pieces)
		allPositions = S.fromList [(ValidPosition x y) | x <- [1..9], y <- [1..9]]

		puttableTypes = map head $ group $ sort $ lookupPair captures side

movingPlays :: PlayerSide -> BoardState -> [Play]
movingPlays side board@(BoardState pieces _) = concatMap generatePlaysFor friendPieces
	where
		generatePlaysFor (posFrom, (_, pieceType)) =
			concatMap playsAt $ destinations board side posFrom
			where
				playsAt posTo
					|promotable posFrom posTo pieceType = [Move posFrom posTo pieceType, Move posFrom posTo $ promote pieceType]
					|otherwise = [Move posFrom posTo pieceType]

		promotable posFrom posTo pieceType =
			(promote pieceType /= pieceType) &&
			(inEnemyTerritory side posFrom || inEnemyTerritory side posTo)

		friendPieces = filter ((==side) . fst . snd) $ M.assocs pieces

-- | Check if given position is enemy's or not.
inEnemyTerritory :: PlayerSide -> ValidPosition -> Bool
inEnemyTerritory Sente (ValidPosition x y) = y <= 3
inEnemyTerritory Gote (ValidPosition x y) = y >= 7


-- | Movable positions considering other pieces but without mates.
destinations :: BoardState -> PlayerSide -> ValidPosition -> [ValidPosition]
destinations (BoardState pieces _) side posFrom = concatMap filterRun runs
	where
		runs = potentialDestinationsInfinite side posFrom piece

		filterRun (p:ps)
			|not (inBoard p) = []
			|otherwise = case M.lookup validP pieces of
				Nothing -> validP : filterRun ps
				Just (blockerSide, blockerPos) ->
					if blockerSide == side
						then []  -- cannot move into friend's place
						else [validP]  -- can take enemy piece, but move past it
			where validP = makePosition p
		filterRun [] = []

		(_, piece) = pieces M.! posFrom
		inBoard (x, y) = 1 <= x && x <= 9 && 1 <= y && y <= 9

-- | Movable positions without considering any other pieces.
potentialDestinationsInfinite :: PlayerSide -> ValidPosition -> Piece -> [[(Int, Int)]]
potentialDestinationsInfinite side pos piece
	|side == Sente = potentialSenteDestinationsInfinite pos piece
	|side == Gote = map (map flipYRaw) $ potentialSenteDestinationsInfinite (flipY pos) piece
	where
		flipYRaw (x, y) = (x, 10 - y)
		flipY (ValidPosition x y) = ValidPosition x (10 - y)

-- | Movable positions without considering any other pieces nor board boundary.
-- Returns "runs", each of which is blocked by a piece.
potentialSenteDestinationsInfinite :: ValidPosition -> Piece -> [[(Int, Int)]]
potentialSenteDestinationsInfinite (ValidPosition x y) FU = [[(x, y - 1)]]
potentialSenteDestinationsInfinite (ValidPosition x y) KY = [[(x, y - i) | i <- [1..]]]
potentialSenteDestinationsInfinite (ValidPosition x y) KE = makeIndependent [(x - 1, y - 2), (x + 1, y - 2)]
potentialSenteDestinationsInfinite (ValidPosition x y) GI = makeIndependent [
	(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
	(x - 1, y + 1), (x + 1, y + 1)]
potentialSenteDestinationsInfinite (ValidPosition x y) KI = makeIndependent [
	(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
	(x - 1, y), (x + 1, y),
	(x, y + 1)]
potentialSenteDestinationsInfinite (ValidPosition x y) KA = [
	[(x - i, y - i) | i <- [1..]],
	[(x - i, y + i) | i <- [1..]],
	[(x + i, y - i) | i <- [1..]],
	[(x + i, y + i) | i <- [1..]]]
potentialSenteDestinationsInfinite (ValidPosition x y) HI = [
	[(x - i, y) | i <- [1..]],
	[(x + i, y) | i <- [1..]],
	[(x, y - i) | i <- [1..]],
	[(x, y + i) | i <- [1..]]]
potentialSenteDestinationsInfinite (ValidPosition x y) OU = makeIndependent [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

potentialSenteDestinationsInfinite pos TO = potentialSenteDestinationsInfinite pos KI
potentialSenteDestinationsInfinite pos NY = potentialSenteDestinationsInfinite pos KI
potentialSenteDestinationsInfinite pos NK = potentialSenteDestinationsInfinite pos KI
potentialSenteDestinationsInfinite pos NG = potentialSenteDestinationsInfinite pos KI
potentialSenteDestinationsInfinite pos@(ValidPosition x y) UM =
	makeIndependent [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] ++
	potentialSenteDestinationsInfinite pos KA
potentialSenteDestinationsInfinite pos@(ValidPosition x y) RY =
	makeIndependent [(x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)] ++
	potentialSenteDestinationsInfinite pos HI

makeIndependent = map (:[])
