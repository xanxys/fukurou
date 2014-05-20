{-# LANGUAGE BangPatterns #-}
module Learn where
import Control.Monad
import System.Directory
import System.FilePath.Posix

targetDirectory :: FilePath
targetDirectory = "./floodgate/2013"

getFilePaths :: IO [FilePath]
getFilePaths = do
	liftM (map (targetDirectory </>) . filter ((/= '.') . head)) $
		getDirectoryContents targetDirectory

doLearn :: IO ()
doLearn = do
	paths <- liftM (take 10) getFilePaths
	print paths


