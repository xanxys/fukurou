-- | Player interacts with CSA-like protocol.
-- http://www.computer-shogi.org/wcsc12/record.html
module CUI where
import Control.Monad
import qualified Data.Map as M
import Data.List

import Base
import Fukurou

-- | Run an interactive human-vs-AI game on CUI.
doCUIGame = do
	isAI@(SengoPair senteIsAI goteIsAI) <- askPlayerInfo
	sente <- createPlayer Sente senteIsAI
	gote <- createPlayer Gote goteIsAI
	let players = (SengoPair sente gote)

	let game = Game (fmap getPlayerName isAI) [] initialBoardState
	mapM_ (flip notifyPlayer game) $ flattenPair players

	progressGUIGame players game
	where
		getPlayerName False = "Human"
		getPlayerName True = "Fukurou"

		createPlayer side False = return Human
		createPlayer side True = liftM AI $ createFukurou side

data CUIPlayer
	= Human
	| AI Fukurou

-- | Return end-game from a game state.
progressGUIGame :: SengoPair CUIPlayer -> Game -> IO Game
progressGUIGame players game = do
	case winningSide game of
		Just side -> finishGame side game >> return game
		Nothing -> continueGame players game

finishGame :: PlayerSide -> Game -> IO ()
finishGame side game = do
	putStrLn $ show side ++ " won!"

-- | Return end-game from a non-finished game state.
continueGame :: SengoPair CUIPlayer -> Game -> IO Game
continueGame players game = do
	putStrLn ""
	putStrLn $ show (1 + length (plays game)) ++ "手目: " ++ show (getTurn game)

	putStr $ showBoard $ latestBoard game
	play <- getPlayFromPlayer $ lookupPair players (getTurn game)
	
	case addPlay play game of
		Nothing -> do
			putStrLn "illegal move; try again"
			continueGame players game
		Just game' -> do
			mapM_ (flip notifyPlayer game') $ flattenPair players
			progressGUIGame players game'

-- | Notify all moves.
notifyPlayer :: CUIPlayer -> Game -> IO ()
notifyPlayer Human _ = return ()
notifyPlayer (AI fukurou) game = Fukurou.notifyPlay fukurou game

getPlayFromPlayer :: CUIPlayer -> IO Play
getPlayFromPlayer Human = CUI.askPlay
getPlayFromPlayer (AI fukurou) = Fukurou.askPlay fukurou


-- | Ask the player an (unconstrained) play. 
askPlay :: IO Play
askPlay = do
	putStrLn "Your play? (CSA-style, e.g. 0055KA=五５角打, 2829TO=二９歩成, resign)"
	answer <- getLine
	case answer of
		"resign" -> return Resign
		(sX:sY:dX:dY:ty) -> return $ Play (read [sX], read [sY]) (read [dX], read [dY]) (read ty)
		_ -> CUI.askPlay

askPlayerInfo :: IO (SengoPair Bool)
askPlayerInfo = do
	senteIsAI <- askYesNo "Sente is AI?"
	goteIsAI <- askYesNo "Gote is AI?"
	return $ SengoPair senteIsAI goteIsAI

askYesNo :: String -> IO Bool
askYesNo question = do
	putStrLn $ question ++ " [yn]"
	answer <- getLine
	case answer of
		"y" -> return True
		"n" -> return False
		_ -> askYesNo question


showPiecesInKanji :: Piece -> String
showPiecesInKanji FU = "歩"
showPiecesInKanji KY = "香"
showPiecesInKanji KE = "桂"
showPiecesInKanji GI = "銀"
showPiecesInKanji KI = "金"
showPiecesInKanji KA = "角"
showPiecesInKanji HI = "飛"
showPiecesInKanji OU = "玉"

showPiecesInKanji TO = "と"
showPiecesInKanji NY = "杏"
showPiecesInKanji NK = "圭"
showPiecesInKanji NG = "全"
showPiecesInKanji UM = "馬"
showPiecesInKanji RY = "龍"

showDigitInKanji :: Int -> String
showDigitInKanji d
	|0 < d && d < 10 = ["一二三四五六七八九" !! (d - 1)]
	|otherwise = error "Digit out of range"

showDigitInZenkakuArabic :: Int -> String
showDigitInZenkakuArabic d
	|0 < d && d < 10 = ["１２３４５６７８９" !! (d - 1)]
	|otherwise = error "Digit out of range"


-- | Convert `BoardState` to multi-line string.
showBoard :: BoardState -> String
showBoard (BoardState onboardPieces (SengoPair sentePieces gotePieces))
	= unlines $ [
		showPosession gotePieces,
		(concat $ map (\column -> " " ++ showDigitInZenkakuArabic column) $ reverse [1..9]),
		horizontalSeparator,
		showBoard onboardPieces ++
		horizontalSeparator,
		showPosession sentePieces]
	where
		horizontalSeparator = replicate (9 * 3 + 2) '-'

		showPosession :: [Piece] -> String
		showPosession pieces = "持駒:" ++ unwords (map showPieceGroup $ group $ sort pieces)
			where
				showPieceGroup pieces@(p:_)
					|length pieces == 1 = showPiecesInKanji p
					|otherwise = showPiecesInKanji p ++ show (length pieces)

		showBoard :: M.Map (Int, Int) (PlayerSide, Piece) -> String
		showBoard board = unlines $ map showRow [1..9]
			where
				showRow row =
					"|" ++
					concatMap (\i -> showPiece (i, row)) (reverse [1..9]) ++
					"|" ++ showDigitInKanji row
				showPiece index = case M.lookup index board of
					Nothing -> " ・"
					Just (Sente, p) -> " " ++ showPiecesInKanji p
					Just (Gote, p) -> "v" ++ showPiecesInKanji p
