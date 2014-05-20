import System.Environment
import Control.Monad

import CUI
import Learn

main = do
	args <- getArgs
	case args of
		["--learn"] -> doLearn
		_ -> void $ doCUIGame
