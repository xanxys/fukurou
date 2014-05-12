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
	(senteIsAI, goteIsAI) <- askPlayerInfo
	sente <- createPlayer Sente senteIsAI
	gote <- createPlayer Gote goteIsAI
	let game = Game (getPlayerName senteIsAI) (getPlayerName goteIsAI) [] initialBoardState
	progressGUIGame sente gote game
	where
		getPlayerName False = "Human"
		getPlayerName True = "Fukurou"

		createPlayer side False = return Human
		createPlayer side True = liftM AI $ createFukurou side

data CUIPlayer
	= Human
	| AI Fukurou

-- | Return end-game from a game state.
progressGUIGame :: CUIPlayer -> CUIPlayer -> Game -> IO Game
progressGUIGame sente gote game = do
	case winningSide game of
		Just side -> finishGame side game >> return game
		Nothing -> continueGame sente gote game

finishGame :: PlayerSide -> Game -> IO ()
finishGame side game = do
	putStrLn $ show side ++ " won!"

-- | Return end-game from a non-finished game state.
continueGame :: CUIPlayer -> CUIPlayer -> Game -> IO Game
continueGame sente gote game = do
	putStrLn ""
	putStrLn $ show (1 + length (plays game)) ++ "手目: " ++ show (getTurn game)

	putStr $ showBoard $ latestBoard game
	play <- case getTurn game of
		Sente -> getPlayFromPlayer sente
		Gote -> getPlayFromPlayer gote
	
	case addPlay play game of
		Nothing -> do
			putStrLn "illegal move; try again"
			continueGame sente gote game
		Just game' -> do
			progressGUIGame sente gote game'


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

askPlayerInfo :: IO (Bool, Bool)
askPlayerInfo = do
	senteIsAI <- askYesNo "Sente is AI?"
	goteIsAI <- askYesNo "Gote is AI?"
	return (senteIsAI, goteIsAI)

askYesNo :: String -> IO Bool
askYesNo question = do
	putStrLn $ question ++ " [yn]"
	answer <- getLine
	case answer of
		"y" -> return True
		"n" -> return False
		_ -> askYesNo question


showPieceJapanese :: Piece -> String
showPieceJapanese FU = "歩"
showPieceJapanese KY = "香"
showPieceJapanese KE = "桂"
showPieceJapanese GI = "銀"
showPieceJapanese KI = "金"
showPieceJapanese KA = "角"
showPieceJapanese HI = "飛"
showPieceJapanese OU = "玉"

showPieceJapanese TO = "と"
showPieceJapanese NY = "杏"
showPieceJapanese NK = "圭"
showPieceJapanese NG = "全"
showPieceJapanese UM = "馬"
showPieceJapanese RY = "龍"

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
showBoard (BoardState board_pieces sente_pieces gote_pieces)
	= unlines $ [
		showPosession sente_pieces,
		(concat $ map (\column -> " " ++ showDigitInZenkakuArabic column) $ reverse [1..9]),
		horizontalSeparator,
		showBoard board_pieces ++
		horizontalSeparator,
		showPosession gote_pieces]
	where
		horizontalSeparator = replicate (9 * 3 + 2) '-'

		showPosession :: [Piece] -> String
		showPosession _ = "持駒:"

		showBoard :: M.Map (Int, Int) (PlayerSide, Piece) -> String
		showBoard board = unlines $ map showRow [1..9]
			where
				showRow row =
					"|" ++
					concatMap (\i -> showPiece (i, row)) (reverse [1..9]) ++
					"|" ++ showDigitInKanji row
				showPiece index = case M.lookup index board of
					Nothing -> " ・"
					Just (Sente, p) -> " " ++ showPieceJapanese p
					Just (Gote, p) -> "v" ++ showPieceJapanese p


