{-# LANGUAGE BangPatterns #-}
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

import Base

type CellState = Word8 

-- | Equivalent to BoardState, but move generation is faster.
data FastBoard = FastBoard !(UArray ValidPosition Word8) !(Array Piece Int) !(Array Piece Int)
	deriving(Eq, Ord)

existFlag :: Word8
existFlag = 0x80

sideFlag :: Word8
sideFlag = 0x40

promoteFlag :: Word8
promoteFlag = 0x20

compressCell :: Maybe (PlayerSide, Piece) -> CellState
compressCell Nothing = 0
compressCell (Just (side, piece)) =
	existFlag .|.
	(if side == Sente then sideFlag else 0) .|.
	(if elem piece [TO, NY, NK, NG, UM, RY] then promoteFlag else 0) .|.
	typeFlag (unpromote piece)
	where
		typeFlag FU = 0
		typeFlag KY = 1
		typeFlag KE = 2
		typeFlag GI = 3
		typeFlag KI = 4
		typeFlag KA = 5
		typeFlag HI = 6
		typeFlag OU = 7

decompressCell :: CellState -> Maybe (PlayerSide, Piece)
decompressCell cs
	|cs .&. existFlag == 0 = Nothing
	|otherwise = Just (side, pieceType)
	where
		side = if cs .&. sideFlag /= 0 then Sente else Gote
		pieceType = (if promoted then promote else id) (baseType $ cs .&. 0x07)
		promoted = cs .&. promoteFlag /= 0
		baseType 0 = FU
		baseType 1 = KY
		baseType 2 = KE
		baseType 3 = GI
		baseType 4 = KI
		baseType 5 = KA
		baseType 6 = HI
		baseType 7 = OU

compressBoard :: BoardState -> FastBoard
compressBoard (BoardState pieces (SengoPair senteCP goteCP)) = FastBoard
	(listArray area $ map (compressCell . flip M.lookup pieces) $ range area)
	(listArray pieceTypeRange $ map (count senteCP) $ range pieceTypeRange)
	(listArray pieceTypeRange $ map (count goteCP) $ range pieceTypeRange)
	where
		count !pieces !target = sum $ map (\x -> if x == target then 1 else 0) pieces
		area = (ValidPosition 1 1, ValidPosition 9 9)
		pieceTypeRange = (FU, HI)

decompressBoard :: FastBoard -> BoardState
decompressBoard _ = undefined

instance Ix Piece where
	range (FU, HI) = [FU,  KY, KE, GI, KI, KA, HI]
	inRange (a, b) x = a <= x && x <= b

instance Ix ValidPosition where
	range (ValidPosition x0 y0, ValidPosition x1 y1) = [ValidPosition x y | x <- [x0..x1], y <- [y0..y1]]
	inRange (ValidPosition x0 y0, ValidPosition x1 y1) (ValidPosition x y) =
		(x0 <= x && x <= x1) &&
		(y0 <= y && y <= y1)

-- | Apply a known-to-be-legal move to given `BoardState`.
updateBoard :: Play -> FastBoard -> FastBoard
updateBoard (Move posFrom posTo pieceTypeTo) (FastBoard pieces senteCaps goteCaps)
	= case decompressCell $ pieces ! posTo of
		-- no capture
		Nothing -> FastBoard pieces' senteCaps goteCaps
		-- capture Sente's stone (Gote's turn)
		Just (Sente, pieceTypeToBeCaptured) -> FastBoard pieces' senteCaps (capture goteCaps pieceTypeToBeCaptured)
		-- capture Gote's stone (Sente's turn)
		Just (Gote, pieceTypeToBeCaptured) -> FastBoard pieces' (capture senteCaps pieceTypeToBeCaptured) goteCaps
	where
		Just (side, _) = decompressCell $ pieces ! posFrom
		pieces' = pieces // [(posFrom, compressCell Nothing), (posTo, compressCell $ Just (side, pieceTypeTo))]
		capture !caps !capType = accum (+) caps [(unpromote capType, 1)]
updateBoard (Put side posTo pieceType) (FastBoard pieces senteCaps goteCaps)
	|side == Sente = FastBoard pieces' (use senteCaps pieceType) goteCaps
	|side == Gote = FastBoard pieces' senteCaps (use goteCaps pieceType)
	where
		use !caps !capType = accum (-) caps [(capType, 1)]
		pieces' = pieces // [(posTo, compressCell $ Just (side, pieceType))]


isCheck :: PlayerSide -> FastBoard -> Bool
isCheck !side state@(FastBoard pieces senteCaps goteCaps) = any takesKing $ enemyMoves
	where
		-- Putting plays cannot capture king, moving plays is enough.
		enemyMoves = FastBoard.movingPlays (flipSide side) state
		takesKing (Move _ dst _) = dst == kingPos
		takesKing (Put _ _ _) = False
		[(kingPos, _)] = filter ((== compressCell (Just (side, OU))) . snd) $ assocs pieces

legalMovesConsideringCheck :: PlayerSide -> FastBoard -> [Play]
legalMovesConsideringCheck side board = filter (not . leadsToCheck) $ FastBoard.legalMoves side board
	where
		leadsToCheck play = FastBoard.isCheck side (FastBoard.updateBoard play board)

-- | Legal moves: movings or puttings of pieces, excluding Resign.
legalMoves :: PlayerSide -> FastBoard -> [Play]
legalMoves side board = FastBoard.movingPlays side board ++ FastBoard.puttingPlays side board

puttingPlays :: PlayerSide -> FastBoard -> [Play]
puttingPlays side (FastBoard pieces senteCaps goteCaps) =
	filter (not . doubleFu) $ 
		[Put side posTo pieceType | posTo <- puttablePositions, pieceType <- puttableTypes]
	where
		doubleFu (Put side (ValidPosition x y) FU) =
			any (\posSearch -> pieces ! posSearch /= compressCell (Just (side, FU)))
			[ValidPosition x ySearch | ySearch <- [1..9]]
		doubleFu _ = False

		puttablePositions = filter ((== compressCell Nothing) . (pieces !)) $ allPositions
		allPositions = [(ValidPosition x y) | x <- [1..9], y <- [1..9]]

		puttableTypes
			|side == Sente = map fst $ filter ((> 0) . snd) $ assocs senteCaps
			|side == Gote = map fst $ filter ((> 0) . snd) $ assocs goteCaps

movingPlays :: PlayerSide -> FastBoard -> [Play]
movingPlays side board@(FastBoard bitboard senteCaps goteCaps) = concatMap generatePlaysFor friendPieces
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

		friendPieces = filter ((==side) . fst . snd) pieces
		pieces = map (id *** fromJust) $ filter (isJust . snd) $ map (id *** decompressCell) $ assocs bitboard


-- | Movable positions considering other pieces but without mates.
destinations :: FastBoard -> PlayerSide -> ValidPosition -> [ValidPosition]
destinations (FastBoard pieces _ _) side posFrom = concatMap filterRun runs
	where
		runs = potentialDestinationsInfinite side posFrom piece

		filterRun (p:ps)
			|not (inBoard p) = []
			|otherwise = case decompressCell (pieces ! validP) of
				Nothing -> validP : filterRun ps
				Just (blockerSide, blockerPos) ->
					if blockerSide == side
						then []  -- cannot move into friend's place
						else [validP]  -- can take enemy piece, but cannot move past it
			where validP = makePosition p
		filterRun [] = []

		Just (_, piece) = decompressCell $ pieces ! posFrom
		inBoard (x, y) = 1 <= x && x <= 9 && 1 <= y && y <= 9
