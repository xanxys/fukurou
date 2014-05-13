{-# LANGUAGE TemplateHaskell #-}
-- | Check if FastBoard's behavior is equivalent to Base implementation.
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Test.QuickCheck
import Test.QuickCheck.All

import Base
import FastBoard

instance Arbitrary PlayerSide where
	arbitrary = elements [Sente, Gote]

instance Arbitrary Piece where
	arbitrary = elements [
		FU , KY , KE , GI , KI, KA, HI,
		TO, NY, NK, NG, UM, RY,
		OU]

instance Arbitrary a => Arbitrary (SengoPair a) where
	arbitrary = SengoPair <$> arbitrary <*> arbitrary

instance Arbitrary ValidPosition where
	arbitrary = ValidPosition <$> index <*> index
		where index =liftM ((+1) . (`mod` 9)) arbitrary

instance Arbitrary BoardState where
	arbitrary = do
		pieces <- arbitrary
		BoardState (M.fromList pieces) <$>
			(SengoPair <$> listOf nonPromotedNormal <*> listOf nonPromotedNormal)

nonPromotedNormal :: Gen Piece
nonPromotedNormal = elements [FU , KY , KE , GI , KI, KA, HI]

normal :: Gen Piece
normal = arbitrary `suchThat` (/= OU)

boardWithKings :: Gen BoardState
boardWithKings = do
	normalPieces <- listOf ((,) <$> arbitrary <*> ((,) <$> arbitrary <*> normal))
	kingSente <- arbitrary
	kingGote <- arbitrary `suchThat` (/= kingSente)
	let piecesWithKings = normalPieces ++ [(kingSente, (Sente, OU)), (kingGote, (Gote, OU))]
	BoardState (M.fromList piecesWithKings) <$>
		(SengoPair <$> listOf nonPromotedNormal <*> listOf nonPromotedNormal)

-- prop_losslessCellCompression cell = (decompressCell . compressCell) cell == cell

-- Actually, Base.BoardState contains more (but, useless) information, namely
-- ordering of captured pieces.
prop_losslessBoardCompression board = (compressBoard . decompressBoard) board' == board'
	where board' = compressBoard board

-- TODO: board is too narrow
prop_movingPlaysEquivalence side =
	forAll boardWithKings $ \board ->
		FastBoard.movingPlays side (compressBoard board) == Base.movingPlays side board

prop_isCheckEquivalence side =
	forAll boardWithKings $ \board ->
		FastBoard.isCheck side (compressBoard board) == Base.isCheck side board
 

main = do
--	$verboseCheckAll
	$quickCheckAll
