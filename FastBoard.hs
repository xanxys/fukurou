{-# LANGUAGE BangPatterns, TupleSections #-}
module FastBoard where
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.Hashable
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Strict.Tuple as ST
import Data.Maybe
import Data.Ord
import Data.Word
import Debug.Trace
import System.Random

import Base

-- | Equivalent to BoardState, but move generation is faster.
-- don't include captured piece type with number=0
data FastBoard = FastBoard (M.Map ValidPosition (ST.Pair PlayerSide Piece)) (SengoPair (M.Map Piece Int))
	deriving(Eq, Ord, Show)


hashPieceBase :: Array (ValidPosition, PlayerSide, Piece) Int
hashPieceBase = listArray
	((ValidPosition 1 1, Sente, FU), (ValidPosition 9 9, Gote, OU))
	$ randoms $ mkStdGen 0

hashCaptureBase :: Array (PlayerSide, Piece, Int) Int
hashCaptureBase = listArray
	((Sente, FU, 0), (Gote, OU, 18))
	$ randoms $ mkStdGen 1

xorSum' :: Bits a => [a] -> a
xorSum' = foldl' xor 0

-- TODO: it's possible to make hash calculation incremental
instance Data.Hashable.Hashable FastBoard where
	hashWithSalt !salt (FastBoard pieces (SengoPair senteCaps goteCaps)) =
		salt `xor` pieceHash `xor` captureHash Sente senteCaps `xor` captureHash Gote goteCaps
		where
			pieceHash = xorSum' [hashPieceBase ! (pos, side, piece) | (pos, (side ST.:!: piece)) <- M.assocs pieces]
			captureHash !side !caps = xorSum' [hashCaptureBase ! (side, piece, num) | (piece, num) <- M.assocs caps]






compressBoard :: BoardState -> FastBoard
compressBoard (BoardState pieces pair) = FastBoard
	(M.map (\(side, piece) -> side ST.:!: piece) pieces) (fmap countGroups pair)
	where
		countGroups !xs =
			M.fromList $! map (\group -> (head group, length group)) $! group xs

decompressBoard :: FastBoard -> BoardState
decompressBoard (FastBoard pieces pair) = BoardState
	(M.map (\(side ST.:!: piece) -> (side, piece)) pieces) (fmap expandGroups pair)
	where
		expandGroups !m =
			concatMap (\(p, n) -> replicate n p) $ M.assocs m

-- | Apply a known-to-be-legal move to given `FastBoard`.
updateBoard :: Play -> FastBoard -> FastBoard
updateBoard (Move posFrom posTo pieceTypeTo) (FastBoard pieces captures)
	= case M.lookup posTo pieces of
		Nothing -> FastBoard pieces' captures  -- no capture
		Just (_ ST.:!: pieceTypeToBeCaptured) -> FastBoard pieces' (captures' $ unpromote pieceTypeToBeCaptured)
	where
		pieces' = M.insert posTo (side ST.:!: pieceTypeTo) $ M.delete posFrom pieces
		captures' newPieceType = partiallyModifyPair side (addCapture newPieceType) captures
		addCapture !pieceType !caps = M.insertWith (+) pieceType 1 caps

		side ST.:!: pieceTypeFrom = pieces M.! posFrom
updateBoard (Put side posTo pieceTypeTo) (FastBoard pieces captures)
	= FastBoard pieces' captures'
	where
		pieces' = M.insert posTo (side ST.:!: pieceTypeTo) pieces
		captures' = partiallyModifyPair side (removeCapture pieceTypeTo) captures
		removeCapture !pieceType !caps = M.update (\num -> if num > 1 then Just (num - 1) else Nothing) pieceType caps


isCheck :: PlayerSide -> FastBoard -> Bool
isCheck !side state@(FastBoard pieces _) = any takesKing $ enemyMoves
	where
		-- Putting plays cannot capture king, moving plays is enough.
		enemyMoves = FastBoard.movingPlays (flipSide side) state
		takesKing (Move _ dst _) = dst == kingPos
		takesKing (Put _ _ _) = False
		[(kingPos, _)] = filter ((== (side ST.:!: OU)) . snd) $ M.assocs pieces

-- | Generate all legal moves in (hopefully) best-first order.
-- (this funcion is implicitly coupled with ev. fun.)
legalMovesConsideringCheck :: PlayerSide -> FastBoard -> [Play]
legalMovesConsideringCheck !side !board
	|FastBoard.isCheck side board = filter (not . leadsToCheck) $
		FastBoard.movingPlays side board ++
		reorderPuttingPlays board (FastBoard.puttingMoves side board)
	|otherwise =
		-- Assumption.
		-- forall piece. piece != OU && piece cannot be taken by enemy
		-- -> moving it will not immediately cause check.
		-- (Further optimization can be done by splitting short run and long run)
		reorderMovingPlays board (filter (not . movingLeadsToCheck) (FastBoard.movingPlays side board)) ++
		reorderPuttingPlays board (FastBoard.puttingMoves side board)
	where
		movingLeadsToCheck play@(Move _ _ OU) = leadsToCheck play
		movingLeadsToCheck play@(Move src _ _)
			|src `S.member` checkedPositions = leadsToCheck play
			|otherwise = False

		checkedPositions = S.fromList [dst | Move _ dst _ <- FastBoard.movingPlays (flipSide side) board]
		leadsToCheck !play = FastBoard.isCheck side (FastBoard.updateBoard play board)


puttingMoves :: PlayerSide -> FastBoard -> [Play]
puttingMoves !side (FastBoard pieces captures) =
	filter (not . stuck) $
	filter (not . doubleFu) $
		[Put side posTo pieceType | posTo <- S.toList puttablePositions, pieceType <- puttableTypes]
	where
		stuck (Put Sente (ValidPosition x y) FU) = y == 1
		stuck (Put Gote (ValidPosition x y) FU) = y == 9
		stuck (Put Sente (ValidPosition x y) KY) = y == 1
		stuck (Put Gote (ValidPosition x y) KY) = y == 9
		stuck (Put Sente (ValidPosition x y) KE) = y <= 2
		stuck (Put Gote (ValidPosition x y) KE) = y >= 8
		stuck _ = False

		doubleFu (Put side (ValidPosition x y) FU) =
			any (\posSearch -> maybe False (== (side ST.:!: FU)) $ M.lookup posSearch pieces)
			[ValidPosition x ySearch | ySearch <- [1..9]]
		doubleFu _ = False

		puttablePositions = allPositions `S.difference` (M.keysSet pieces)
		allPositions = S.fromList [(ValidPosition x y) | x <- [1..9], y <- [1..9]]

		puttableTypes = M.keys $ lookupPair captures side

movingPlays :: PlayerSide -> FastBoard -> [Play]
movingPlays !side board@(FastBoard pieces _) = concatMap generatePlaysFor friendPieces
	where
		generatePlaysFor (posFrom, (_ ST.:!: pieceType))
			|isPromotable pieceType && inEnemyTerritory side posFrom =
				map promotingPlayAt dests ++ nonPromotingNonStuckPlays
			|isPromotable pieceType =
				mapMaybe maybePromotingPlayAt dests ++ nonPromotingNonStuckPlays
			|otherwise = nonPromotingNonStuckPlays
			where
				dests = FastBoard.destinations board side posFrom

				-- cf. promoting plays are guranteed to be non-stuck
				nonPromotingNonStuckPlays = filter (not . stuck side) $! map nonPromotingPlayAt dests

				nonPromotingPlayAt !posTo = Move posFrom posTo pieceType
				promotingPlayAt !posTo = Move posFrom posTo promotedType
				maybePromotingPlayAt !posTo
					|inEnemyTerritory side posTo = Just $! Move posFrom posTo promotedType
					|otherwise = Nothing
				promotedType = promote pieceType

		stuck Sente (Move _ (ValidPosition x y) FU) = y == 1
		stuck Gote (Move _ (ValidPosition x y) FU) = y == 9
		stuck Sente (Move _ (ValidPosition x y) KY) = y == 1
		stuck Gote (Move _ (ValidPosition x y) KY) = y == 9
		stuck Sente (Move _ (ValidPosition x y) KE) = y <= 2
		stuck Gote (Move _ (ValidPosition x y) KE) = y >= 8
		stuck _ _ = False

		friendPieces = filter ((==side) . ST.fst . snd) $ M.assocs pieces

reorderPuttingPlays :: FastBoard -> [Play] -> [Play]
reorderPuttingPlays board@(FastBoard pieces _) !plays =
	reverse $! sortBy (comparing threatens) plays
	where
		threatens play@(Put side dst _) =
			sum $ map posToThreat targets
			where
				board' = FastBoard.updateBoard play board
				targets = FastBoard.destinations board' side dst

				posToThreat !pos = case M.lookup pos pieces of
					Nothing -> 0
					Just (targetSide ST.:!: _) ->
						if targetSide == side then 0 else 1


-- | A play to capture enemy's piece is mostly better than others.
reorderMovingPlays :: FastBoard -> [Play] -> [Play]
reorderMovingPlays (FastBoard pieces _) !plays =
	reverse $! sortBy (comparing takesPiece) plays
	where
		takesPiece (Move _ dst _) = M.member dst pieces


isPromotable :: Piece -> Bool
isPromotable !p = (p <= HI) && (p /= KI)

-- | Movable positions considering other pieces but without mates.
destinations :: FastBoard -> PlayerSide -> ValidPosition -> [ValidPosition]
destinations (FastBoard pieces _) !side !posFrom = concatMap filterRun runs
	where
		runs = potentialDestionationsInfiniteTable ! (side, piece, posFrom)

		filterRun (!p : ps) = case M.lookup p pieces of
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
