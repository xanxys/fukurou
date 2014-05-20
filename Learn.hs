{-# LANGUAGE BangPatterns #-}
module Learn where
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map
import qualified Data.Packed
import Numeric.Container
import System.Directory
import System.FilePath.Posix

import Base

-- | Learn weights of the evaluation function from kifu.
-- The learning method is (probably) very similar to Bonanza's.
--
-- eval(B) = sum (delta(relation in B) * weight(relation) for relation)
--
-- If the evaluation function is good, the following always holds:
--   eval(best move) > eval(other move)
--
-- We optimize weight by minimizing J:
--   J = sum (eval(B * other move) - eval(B * best move)) + (pawn - 100)^2
--     = sum (dot(W, rels in B * other move - rels in B * best move)) + ...
--
-- dJ/dW = sum (rels in B * other move - rels in B * best move) +
--  + 2 * (pawn - 100)
-- the second term tries to fix FU's value to 100. (regularizer)
doLearn :: IO ()
doLearn = do
	games <- collectRecords "./floodgate/2013"
	let samples = concatMap extractSamples games
	let weight = learnWeight samples
	print weight
	return ()

learnWeight :: [(PlayerSide, BoardState, [BoardState])] -> FeatureVector
learnWeight samples = scale (1 / fromIntegral numPairs) deltaFV
	where
		numPairs = sum' [length bads | (side, good, bads) <- samples]
		deltaFV = sumV1' [scale (if side == Sente then 1 else -1) $ sumV1' $ map (\bad -> extractFeatures bad `sub` extractFeatures good) bads | (side, good, bads) <- samples, not $ null bads]

sumV1' :: Container c e => [c e] -> c e
sumV1' = foldl1' add

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

type FeatureVector = Data.Packed.Vector Float

-- | Extract for Sente
extractFeatures :: BoardState -> FeatureVector
extractFeatures (BoardState pieces (SengoPair senteCP goteCP)) =
	Data.Packed.fromList $ map count [FU .. RY]
	where
		count ty =
			sum' (map (\(side, t) -> delta t ty * sideS side) ps) +
			sum' (map (delta ty) senteCP) - sum' (map (delta ty) goteCP)
		delta !x !y
			|x == y = 1
			|x /= y = 0
		ps = Data.Map.elems pieces

		sideS Sente = 1
		sideS Gote = -1

{-
			(FU, 100),
			(KY, 300),
			(KE, 400),
			(GI, 500),
			(KI, 600),
			(KA, 800),
			(HI, 1000),
			(TO, 700),
			(NY, 680),
			(NK, 660),
			(NG, 640),
			(UM, 1100),
			(RY, 1200),
			(OU, 5000)]
-}


-- | Extract (state after a single (hopefully) best move, bunch of states after legal bad moves)
-- from each board state. This function assumes that the winning side played
-- perfect game.
extractSamples :: Game -> [(PlayerSide, BoardState, [BoardState])]
extractSamples game = case winningSide game of
	Nothing -> []
	Just winner -> map expandState $ [(state, side, play, filter (/= play) $ legalMovesConsideringCheck side state) |
		(state, side, play) <- states (reverse $ plays game) Sente initialBoardState,
		side == winner]
	where
		expandState (s0, side, play, badPlays) =
			(side, updateBoard play s0, map (flip updateBoard s0) badPlays)
		states [] _ _ = []
		states (Resign:_) _ _ = []
		states (p:ps) side state =
			(state, side, p) : states ps (flipSide side) (updateBoard p state)
		winner = winningSide game


collectRecords :: FilePath -> IO [Game]
collectRecords targetDirectory = do
	paths <- liftM (take 2) $ getFilePaths targetDirectory
	conts <- mapM readFile paths
	return $! mapMaybe parseCSA conts


getFilePaths :: FilePath -> IO [FilePath]
getFilePaths targetDirectory = do
	liftM (map (targetDirectory </>) . filter ((/= '.') . head)) $
		getDirectoryContents targetDirectory


-- | Parse CSA kifu (http://www.computer-shogi.org/protocol/record_v21.html)
parseCSA :: String -> Maybe Game
parseCSA !content = do
	return $! foldl' parse record0 (lines content)
	where
		record0 = Game {
			playerNames = SengoPair "?" "?",
			plays = [],
			latestBoard = initialBoardState
		}
		parse !record ('N':side:name) = record {
			playerNames = partiallyModifyPair (toSide side) (const name) (playerNames record)
		}
		parse !record maybePlay = case parsePlay maybePlay of
			Nothing -> record
			Just play -> case addPlay play record of
				Just record' -> record'
				Nothing -> error $ "Invalid move: " ++ maybePlay ++ " against\n" ++ show record

		toSide '+' = Sente
		toSide '-' = Gote

		parsePlay !play = case play of
			"%TORYO" -> return Resign
			('+':'0':'0':dX:dY:ty) -> return $ Put
				Sente (makePosition (read [dX], read [dY])) (read ty)
			('-':'0':'0':dX:dY:ty) -> return $ Put
				Gote (makePosition (read [dX], read [dY])) (read ty)
			('+':sX:sY:dX:dY:ty) -> return $ Move
				(makePosition (read [sX], read [sY]))
				(makePosition (read [dX], read [dY])) (read ty)
			('-':sX:sY:dX:dY:ty) -> return $ Move
				(makePosition (read [sX], read [sY]))
				(makePosition (read [dX], read [dY])) (read ty)
			_ -> Nothing
