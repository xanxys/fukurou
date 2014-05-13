{-# LANGUAGE BangPatterns, TupleSections #-}
module FastBoard where
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Strict.Tuple as ST
import Data.Maybe
import Data.Ord
import Data.Word
import Debug.Trace

import Base

-- | Equivalent to BoardState, but move generation is faster.
data FastBoard = FastBoard (M.Map ValidPosition (ST.Pair PlayerSide Piece)) (SengoPair [Piece])
	deriving(Eq, Ord, Show)

compressBoard :: BoardState -> FastBoard
compressBoard (BoardState pieces pair) = FastBoard
	(M.map (\(side, piece) -> side ST.:!: piece) pieces) pair

decompressBoard :: FastBoard -> BoardState
decompressBoard (FastBoard pieces pair) = BoardState
	(M.map (\(side ST.:!: piece) -> (side, piece)) pieces) pair

-- | Apply a known-to-be-legal move to given `FastBoard`.
updateBoard :: Play -> FastBoard -> FastBoard
updateBoard (Move posFrom posTo pieceTypeTo) (FastBoard pieces captures)
	= case M.lookup posTo pieces of
		Nothing -> FastBoard pieces' captures  -- no capture
		Just (_ ST.:!: pieceTypeToBeCaptured) -> FastBoard pieces' (captures' $ unpromote pieceTypeToBeCaptured)
	where
		pieces' = M.insert posTo (side ST.:!: pieceTypeTo) $ M.delete posFrom pieces
		captures' newPieceType = partiallyModifyPair side (newPieceType:) captures

		side ST.:!: pieceTypeFrom = pieces M.! posFrom
updateBoard (Put side posTo pieceTypeTo) (FastBoard pieces captures)
	= FastBoard pieces' captures'
	where
		pieces' = M.insert posTo (side ST.:!: pieceTypeTo) pieces
		captures' = partiallyModifyPair side (subtractCapture pieceTypeTo) captures


isCheck :: PlayerSide -> FastBoard -> Bool
isCheck !side state@(FastBoard pieces _) = any takesKing $ enemyMoves
	where
		-- Putting plays cannot capture king, moving plays is enough.
		enemyMoves = FastBoard.movingPlays (flipSide side) state
		takesKing (Move _ dst _) = dst == kingPos
		takesKing (Put _ _ _) = False
		[(kingPos, _)] = filter ((== (side ST.:!: OU)) . snd) $ M.assocs pieces

legalMovesConsideringCheck :: PlayerSide -> FastBoard -> [Play]
legalMovesConsideringCheck !side !board
	|FastBoard.isCheck side board = filter (not . leadsToCheck) $
		FastBoard.movingPlays side board ++ FastBoard.puttingMoves side board
	|otherwise = 
		filter (not . leadsToCheck) (FastBoard.movingPlays side board) ++
		FastBoard.puttingMoves side board
	where
		leadsToCheck play = FastBoard.isCheck side (FastBoard.updateBoard play board)


puttingMoves :: PlayerSide -> FastBoard -> [Play]
puttingMoves !side (FastBoard pieces captures) =
	filter (not . doubleFu) $ 
		[Put side posTo pieceType | posTo <- S.toList puttablePositions, pieceType <- puttableTypes]
	where
		doubleFu (Put side (ValidPosition x y) FU) =
			any (\posSearch -> maybe False (== (side ST.:!: FU)) $ M.lookup posSearch pieces)
			[ValidPosition x ySearch | ySearch <- [1..9]]
		doubleFu _ = False

		puttablePositions = allPositions `S.difference` (M.keysSet pieces)
		allPositions = S.fromList [(ValidPosition x y) | x <- [1..9], y <- [1..9]]

		puttableTypes = map head $ group $ sort $ lookupPair captures side

movingPlays :: PlayerSide -> FastBoard -> [Play]
movingPlays !side board@(FastBoard pieces _) = concatMap generatePlaysFor friendPieces
	where
		generatePlaysFor (posFrom, (_ ST.:!: pieceType)) =
			concatMap playsAt $ FastBoard.destinations board side posFrom
			where
				playsAt !posTo
					|isPiecePromotable && isPositionPromotable posFrom posTo =
						[Move posFrom posTo pieceType, Move posFrom posTo promotedType]
					|otherwise = [Move posFrom posTo pieceType]
				isPiecePromotable = isPromotable pieceType
				promotedType = promote pieceType

		isPositionPromotable posFrom posTo =
			(inEnemyTerritory side posFrom || inEnemyTerritory side posTo)

		friendPieces = filter ((==side) . ST.fst . snd) $ M.assocs pieces

isPromotable :: Piece -> Bool
isPromotable !p = (p <= HI) && (p /= KI)

-- | Movable positions considering other pieces but without mates.
destinations :: FastBoard -> PlayerSide -> ValidPosition -> [ValidPosition]
destinations (FastBoard pieces _) !side !posFrom = concatMap filterRun runs
	where
		runs = potentialDestionationsInfiniteTable ! (side, piece, posFrom)

		filterRun (p:ps) = case M.lookup p pieces of
				Nothing -> p : filterRun ps
				Just (blockerSide ST.:!: blockerPos) ->
					if blockerSide == side
						then []  -- cannot move into friend's place
						else [p]  -- can take enemy piece, but cannot move past it
		filterRun [] = []

		_ ST.:!: piece = pieces M.! posFrom


potentialDestionationsInfiniteTable :: Array (PlayerSide, Piece, ValidPosition) [[ValidPosition]]
potentialDestionationsInfiniteTable =
	listArray combinations $! map generateFiniteRuns $! range combinations
	where
		generateFiniteRuns (side, piece, posFrom) =
			map (map makePosition . takeWhile inBoard) $
				potentialDestinationsInfinite side posFrom piece
		combinations = ((Sente, FU, ValidPosition 1 1), (Gote, OU, ValidPosition 9 9))
		inBoard (x, y) = 1 <= x && x <= 9 && 1 <= y && y <= 9
