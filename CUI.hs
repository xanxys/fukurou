module CUI where
import qualified Data.Map as M
import Data.List

import Base

doCUIGame = do
	oppSide <- askOpponentSide
	putStr $ showBoard initialBoardState
	askPlay

askPlay :: IO Play
askPlay = do
	putStrLn "Your play? (CSA-style, e.g. 0055KA=五５角打 2829TO=二９歩成)"
	answer <- getLine
	case answer of
		(sX:sY:dX:dY:ty) -> return $ Play (read [sX], read [sY]) (read [dX], read [dY]) (read ty)
		_ -> askPlay

askOpponentSide :: IO PlayerSide
askOpponentSide = do
	putStrLn "You're sente? [yn]"
	answer <- getLine
	case answer of
		"y" -> return Sente
		"n" -> return Gote
		_ -> askOpponentSide


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


