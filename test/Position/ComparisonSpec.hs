module Position.ComparisonSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Expectation
import Position
import Position.Comparison
import Position.Offset
import PositionArbitraries

boundedOffset :: Gen Int
boundedOffset = oneof [choose (-14, -1), choose (1, 14)]

positionWithOffset :: Gen (Position, Int)
positionWithOffset = (,) <$> arbitrary <*> boundedOffset

positionWithOffset2 :: Gen (Position, Int, Int)
positionWithOffset2 = (,,) <$> arbitrary <*> boundedOffset <*> boundedOffset

horizontalOffset :: Gen (Position, Position)
horizontalOffset = positionWithOffset `suchThatMap` (\(p, d) -> (,) p <$> horizontal d p)

verticalOffset :: Gen (Position, Position)
verticalOffset = positionWithOffset `suchThatMap` (\(p, d) -> (,) p <$> vertical d p)

mainDiagonalOffset :: Gen (Position, Position)
mainDiagonalOffset = positionWithOffset `suchThatMap` (\(p, d) -> (,) p <$> mainDiagonal d p)

antiDiagonalOffset :: Gen (Position, Position)
antiDiagonalOffset = positionWithOffset `suchThatMap` (\(p, d) -> (,) p <$> antiDiagonal d p)

orthogonalOffset :: Gen (Position, Position)
orthogonalOffset = oneof [horizontalOffset, verticalOffset]

diagonalOffset :: Gen (Position, Position)
diagonalOffset = oneof [mainDiagonalOffset, antiDiagonalOffset]

alignedOffset :: Gen (Position, Position)
alignedOffset = oneof [orthogonalOffset, diagonalOffset]

unalignedOffset :: Gen (Position, Position)
unalignedOffset = positionWithOffset2 `suchThat` (\(_, d, d') -> abs d /= abs d') `suchThatMap` (\(p, d, d') -> (,) p <$> (horizontal d p >>= vertical d'))

spec :: Spec
spec = do
  describe "isHorizontal" $ do
    context "given identical positions" $ do
      it "returns True" $ property $ \p ->
        isHorizontal p p `shouldBe` True
    context "given horizontally offset positions" $ do
      it "returns True" $ property $
        forAll horizontalOffset $ \(p, q) ->
          isHorizontal p q `shouldBe` True
    context "given vertically offset positions" $ do
      it "returns False" $ property $
        forAll verticalOffset $ \(p, q) ->
          isHorizontal p q `shouldBe` False
    context "given main diagonally offset positions" $ do
      it "returns False" $ property $
        forAll mainDiagonalOffset $ \(p, q) ->
          isHorizontal p q `shouldBe` False
    context "given anti diagonally offset positions" $ do
      it "returns False" $ property $
        forAll antiDiagonalOffset $ \(p, q) ->
          isHorizontal p q `shouldBe` False
    context "given unaligned offset positions" $ do
      it "returns False" $ property $
        forAll unalignedOffset $ \(p, q) ->
          isHorizontal p q `shouldBe` False
  describe "isVertical" $ do
    context "given identical positions" $ do
      it "returns True" $ property $ \p ->
        isVertical p p `shouldBe` True
    context "given vertically offset positions" $ do
      it "returns True" $ property $
        forAll verticalOffset $ \(p, q) ->
          isVertical p q `shouldBe` True
    context "given horizontally offset positions" $ do
      it "returns False" $ property $
        forAll horizontalOffset $ \(p, q) ->
          isVertical p q `shouldBe` False
    context "given main diagonally offset positions" $ do
      it "returns False" $ property $
        forAll mainDiagonalOffset $ \(p, q) ->
          isVertical p q `shouldBe` False
    context "given anti diagonally offset positions" $ do
      it "returns False" $ property $
        forAll antiDiagonalOffset $ \(p, q) ->
          isVertical p q `shouldBe` False
    context "given unaligned offset positions" $ do
      it "returns False" $ property $
        forAll unalignedOffset $ \(p, q) ->
          isVertical p q `shouldBe` False
  describe "isOrthogonal" $ do
    context "given identical positions" $ do
      it "returns True" $ property $ \p ->
        isOrthogonal p p `shouldBe` True
    context "given orthogonally offset positions" $ do
      it "returns True" $ property $
        forAll orthogonalOffset $ \(p, q) ->
          isOrthogonal p q `shouldBe` True
    context "given diagonally offset positions" $ do
      it "returns False" $ property $
        forAll diagonalOffset $ \(p, q) ->
          isOrthogonal p q `shouldBe` False
    context "given unaligned offset positions" $ do
      it "returns False" $ property $
        forAll unalignedOffset $ \(p, q) ->
          isOrthogonal p q `shouldBe` False
  describe "isMainDiagonal" $ do
    context "given identical positions" $ do
      it "returns True" $ property $ \p ->
        isMainDiagonal p p `shouldBe` True
    context "given main diagonally offset positions" $ do
      it "returns True" $ property $
        forAll mainDiagonalOffset $ \(p, q) ->
          isMainDiagonal p q `shouldBe` True
    context "given horizontally offset positions" $ do
      it "returns False" $ property $
        forAll horizontalOffset $ \(p, q) ->
          isMainDiagonal p q `shouldBe` False
    context "given vertically offset positions" $ do
      it "returns False" $ property $
        forAll verticalOffset $ \(p, q) ->
          isMainDiagonal p q `shouldBe` False
    context "given anti diagonally offset positions" $ do
      it "returns False" $ property $
        forAll antiDiagonalOffset $ \(p, q) ->
          isMainDiagonal p q `shouldBe` False
    context "given unaligned offset positions" $ do
      it "returns False" $ property $
        forAll unalignedOffset $ \(p, q) ->
          isMainDiagonal p q `shouldBe` False
  describe "isAntiDiagonal" $ do
    context "given identical positions" $ do
      it "returns True" $ property $ \p ->
        isAntiDiagonal p p `shouldBe` True
    context "given anti diagonally offset positions" $ do
      it "returns True" $ property $
        forAll antiDiagonalOffset $ \(p, q) ->
          isAntiDiagonal p q `shouldBe` True
    context "given horizontally offset positions" $ do
      it "returns False" $ property $
        forAll horizontalOffset $ \(p, q) ->
          isAntiDiagonal p q `shouldBe` False
    context "given vertically offset positions" $ do
      it "returns False" $ property $
        forAll verticalOffset $ \(p, q) ->
          isAntiDiagonal p q `shouldBe` False
    context "given main diagonally offset positions" $ do
      it "returns False" $ property $
        forAll mainDiagonalOffset $ \(p, q) ->
          isAntiDiagonal p q `shouldBe` False
    context "given unaligned offset positions" $ do
      it "returns False" $ property $
        forAll unalignedOffset $ \(p, q) ->
          isAntiDiagonal p q `shouldBe` False
  describe "isDiagonal" $ do
    context "given identical positions" $ do
      it "returns True" $ property $ \p ->
        isDiagonal p p `shouldBe` True
    context "given diagonally offset positions" $ do
      it "returns True" $ property $
        forAll diagonalOffset $ \(p, q) ->
          isDiagonal p q `shouldBe` True
    context "given orthogonally offset positions" $ do
      it "returns False" $ property $
        forAll orthogonalOffset $ \(p, q) ->
          isDiagonal p q `shouldBe` False
    context "given unaligned offset positions" $ do
      it "returns False" $ property $
        forAll unalignedOffset $ \(p, q) ->
          isDiagonal p q `shouldBe` False
  describe "isAligned" $ do
    context "given identical positions" $ do
      it "returns True" $ property $ \p ->
        isAligned p p `shouldBe` True
    context "given aligned offset positions" $ do
      it "returns True" $ property $
        forAll alignedOffset $ \(p, q) ->
          isAligned p q `shouldBe` True
    context "given unaligned offset positions" $ do
      it "returns False" $ property $
        forAll unalignedOffset $ \(p, q) ->
          isAligned p q `shouldBe` False
  describe "distance" $ do
    it "" $ property $ \p q ->
      distance p q `shouldBe` distance q p
    it "satisfies the triangle inequality" $ property $ \p q r ->
      distance p r `shouldSatisfy` (<= distance p q + distance q r)
    context "given identical positions" $ do
      it "returns 0" $ property $ \p ->
        distance p p `shouldBe` 0
    context "given horizontally offset positions" $ do
      it "returns the absolute value of the offset" $ property $ \d p ->
        distance p <$> horizontal d p `shouldBeMaybe` abs d
    context "given vertically offset positions" $ do
      it "returns the absolute value of the offset" $ property $ \d p ->
        distance p <$> vertical d p `shouldBeMaybe` abs d
    context "given main diagonally offset positions" $ do
      it "returns the absolute value of the offset" $ property $ \d p ->
        distance p <$> mainDiagonal d p `shouldBeMaybe` abs d
    context "given anti diagonally offset positions" $ do
      it "returns the absolute value of the offset" $ property $ \d p ->
        distance p <$> antiDiagonal d p `shouldBeMaybe` abs d
