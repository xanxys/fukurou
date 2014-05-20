{-# LANGUAGE BangPatterns #-}
module Learn where
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath.Posix

import Base

targetDirectory :: FilePath
targetDirectory = "./floodgate/2013"

getFilePaths :: IO [FilePath]
getFilePaths = do
	liftM (map (targetDirectory </>) . filter ((/= '.') . head)) $
		getDirectoryContents targetDirectory

doLearn :: IO ()
doLearn = do
	paths <- liftM (take 10) getFilePaths
	conts <- mapM readFile paths
	print $ map parseCSA conts



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
