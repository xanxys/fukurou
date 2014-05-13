{-# LANGUAGE BangPatterns, TupleSections #-}
module FastBoard where
import Control.Arrow
import Control.Monad
import Data.Array.Unboxed
import Data.Bits
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Ord
import Data.Word
import Debug.Trace

import Base

type CellState = Word8 

-- | Equivalent to BoardState, but move generation is faster.
data FastBoard = FastBoard (M.Map ValidPosition (PlayerSide, Piece)) (SengoPair [Piece])
	deriving(Eq, Ord, Show)

existFlag :: Word8
existFlag = 0x80

sideFlag :: Word8
sideFlag = 0x40

promoteFlag :: Word8
promoteFlag = 0x20

typeMask :: Word8
typeMask = 0x07


compressBoard :: BoardState -> FastBoard
compressBoard (BoardState pieces pair) = FastBoard pieces pair

decompressBoard :: FastBoard -> BoardState
decompressBoard (FastBoard pieces pair) = BoardState pieces pair

instance Ix Piece where
	range (FU, HI) = [FU, KY, KE, GI, KI, KA, HI]
	inRange (a, b) x = a <= x && x <= b
	index (FU, _) FU = 0
	index (FU, _) KY = 1
	index (FU, _) KE = 2
	index (FU, _) GI = 3
	index (FU, _) KI = 4
	index (FU, _) KA = 5
	index (FU, _) HI = 6

{-
instance Ix ValidPosition where
	range (ValidPosition x0 y0, ValidPosition x1 y1) = [ValidPosition x y | x <- [x0..x1], y <- [y0..y1]]
	index (ValidPosition x0 y0, ValidPosition x1 y1) (ValidPosition x y) = (x - x0) * (y1 - y0 + 1) + (y - y0)
	inRange (ValidPosition x0 y0, ValidPosition x1 y1) (ValidPosition x y) =
		(x0 <= x && x <= x1) && (y0 <= y && y <= y1)
-}


-- | Apply a known-to-be-legal move to given `FastBoard`.
updateBoard :: Play -> FastBoard -> FastBoard
updateBoard (Move posFrom posTo pieceTypeTo) (FastBoard pieces captures)
	= case M.lookup posTo pieces of
		Nothing -> FastBoard pieces' captures  -- no capture
		Just (_, pieceTypeToBeCaptured) -> FastBoard pieces' (captures' $ unpromote pieceTypeToBeCaptured)
	where
		pieces' = M.insert posTo (side, pieceTypeTo) $ M.delete posFrom pieces
		captures' newPieceType = partiallyModifyPair side (newPieceType:) captures

		(side, pieceTypeFrom) = pieces M.! posFrom
updateBoard (Put side posTo pieceTypeTo) (FastBoard pieces captures)
	= FastBoard pieces' captures'
	where
		pieces' = M.insert posTo (side, pieceTypeTo) pieces
		captures' = partiallyModifyPair side (subtractCapture pieceTypeTo) captures

-- TODO: consider draw case.
-- TODO: implement check-mate
winningSide :: Game -> Maybe PlayerSide
winningSide game
	|not (null $ plays game) && head (plays game) == Resign = Just $ getTurn game
	|otherwise = Nothing

initialFastBoard :: FastBoard
initialFastBoard = FastBoard (M.fromList pairs) (SengoPair [] [])
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

isCheck :: PlayerSide -> FastBoard -> Bool
isCheck side state@(FastBoard pieces _) = any takesKing $ enemyMoves
	where
		-- Putting plays cannot capture king, moving plays is enough.
		enemyMoves = FastBoard.movingPlays (flipSide side) state
		takesKing (Move _ dst _) = dst == kingPos
		takesKing (Put _ _ _) = False
		[(kingPos, _)] = filter ((== (side, OU)) . snd) $ M.assocs pieces

legalMovesConsideringCheck :: PlayerSide -> FastBoard -> [Play]
legalMovesConsideringCheck side board = filter (not . leadsToCheck) $ FastBoard.legalMoves side board
	where
		leadsToCheck play = FastBoard.isCheck side (FastBoard.updateBoard play board)

-- | Legal moves: movings or puttings of pieces, excluding Resign.
legalMoves :: PlayerSide -> FastBoard -> [Play]
legalMoves side board = FastBoard.movingPlays side board ++ FastBoard.puttingMoves side board

puttingMoves :: PlayerSide -> FastBoard -> [Play]
puttingMoves side (FastBoard pieces captures) =
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

movingPlays :: PlayerSide -> FastBoard -> [Play]
movingPlays side board@(FastBoard pieces _) = concatMap generatePlaysFor friendPieces
	where
		generatePlaysFor (posFrom, (_, pieceType)) =
			concatMap playsAt $ FastBoard.destinations board side posFrom
			where
				playsAt posTo
					|promotable posFrom posTo pieceType = [Move posFrom posTo pieceType, Move posFrom posTo $ promote pieceType]
					|otherwise = [Move posFrom posTo pieceType]

		promotable posFrom posTo pieceType =
			(promote pieceType /= pieceType) &&
			(inEnemyTerritory side posFrom || inEnemyTerritory side posTo)

		friendPieces = filter ((==side) . fst . snd) $ M.assocs pieces


-- | Movable positions considering other pieces but without mates.
destinations :: FastBoard -> PlayerSide -> ValidPosition -> [ValidPosition]
destinations (FastBoard pieces _) side posFrom = concatMap filterRun runs
	where
		runs = potentialDestinationsInfinite side posFrom piece

		filterRun (p:ps)
			|not (inBoard p) = []
			|otherwise = case M.lookup validP pieces of
				Nothing -> validP : filterRun ps
				Just (blockerSide, blockerPos) ->
					if blockerSide == side
						then []  -- cannot move into friend's place
						else [validP]  -- can take enemy piece, but cannot move past it
			where validP = makePosition p
		filterRun [] = []

		(_, piece) = pieces M.! posFrom
		inBoard (x, y) = 1 <= x && x <= 9 && 1 <= y && y <= 9
