module PositionSpec (spec) where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import Position
import PositionArbitraries

spec :: Spec
spec = do
  describe "isValid" $ do
    context "given (0, 0)" $ do
      it "returns False" $ do
        isValid 0 0 `shouldBe` False
    context "given (4, 0)" $ do
      it "returns False" $ do
        isValid 4 0 `shouldBe` False
    context "given (5, 0)" $ do
      it "returns True" $ do
        isValid 5 0 `shouldBe` True
    context "given (9, 0)" $ do
      it "returns True" $ do
        isValid 9 0 `shouldBe` True
    context "given (10, 0)" $ do
      it "returns False" $ do
        isValid 10 0 `shouldBe` False
    context "given (14, 0)" $ do
      it "returns False" $ do
        isValid 14 0 `shouldBe` False
    context "given (0, 14)" $ do
      it "returns False" $ do
        isValid 0 14 `shouldBe` False
    context "given (4, 14)" $ do
      it "returns False" $ do
        isValid 4 14 `shouldBe` False
    context "given (5, 14)" $ do
      it "returns True" $ do
        isValid 5 14 `shouldBe` True
    context "given (9, 14)" $ do
      it "returns True" $ do
        isValid 9 14 `shouldBe` True
    context "given (10, 14)" $ do
      it "returns False" $ do
        isValid 10 14 `shouldBe` False
    context "given (14, 14)" $ do
      it "returns False" $ do
        isValid 14 14 `shouldBe` False
    context "given (0, 0)" $ do
      it "returns False" $ do
        isValid 0 0 `shouldBe` False
    context "given (0, 4)" $ do
      it "returns False" $ do
        isValid 0 4 `shouldBe` False
    context "given (0, 5)" $ do
      it "returns True" $ do
        isValid 0 5 `shouldBe` True
    context "given (0, 9)" $ do
      it "returns True" $ do
        isValid 0 9 `shouldBe` True
    context "given (0, 10)" $ do
      it "returns False" $ do
        isValid 0 10 `shouldBe` False
    context "given (0, 14)" $ do
      it "returns False" $ do
        isValid 0 14 `shouldBe` False
    context "given (14, 0)" $ do
      it "returns False" $ do
        isValid 14 0 `shouldBe` False
    context "given (14, 4)" $ do
      it "returns False" $ do
        isValid 14 4 `shouldBe` False
    context "given (14, 5)" $ do
      it "returns True" $ do
        isValid 14 5 `shouldBe` True
    context "given (14, 9)" $ do
      it "returns True" $ do
        isValid 14 9 `shouldBe` True
    context "given (14, 10)" $ do
      it "returns False" $ do
        isValid 14 10 `shouldBe` False
    context "given (14, 14)" $ do
      it "returns False" $ do
        isValid 14 14 `shouldBe` False
    context "given a x < 0" $ do
      it "returns False" $ property $ \y ->
        forAll (choose (minBound, -1)) $ \x ->
          isValid x y `shouldBe` False
    context "given x > 14" $ do
      it "returns False" $ property $ \y ->
        forAll (choose (15, maxBound)) $ \x ->
          isValid x y `shouldBe` False
    context "given y < 0" $ do
      it "returns False" $ property $ \x ->
        forAll (choose (minBound, -1)) $ \y ->
          isValid x y `shouldBe` False
    context "given y > 14" $ do
      it "returns False" $ property $ \x ->
        forAll (choose (15, maxBound)) $ \y ->
          isValid x y `shouldBe` False
    context "given 0 <= x <= 14 and 5 <= y <= 9" $ do
      it "returns True" $ property $
        forAll (choose (0, 14)) $ \x ->
          forAll (choose (5, 9)) $ \y ->
            isValid x y `shouldBe` True
    context "given 5 <= x <= 9 and 0 <= y <= 14" $ do
      it "returns True" $ property $
        forAll (choose (5, 9)) $ \x ->
          forAll (choose (0, 14)) $ \y ->
            isValid x y `shouldBe` True
  describe "position" $ do
    it "inverts unPosition" $ property $ \p ->
      uncurry position (unPosition p) `shouldBe` Just p
    context "given invalid coordinates" $ do
      it "returns nothing" $ property $
        forAll invalidCoordinates $ \(x, y) ->
          position x y `shouldBe` Nothing
    context "given valid coordinates" $ do
      it "returns a Position with the given x coordinate" $ property $
        forAll validCoordinates $ \(x, y) ->
          fmap getX (position x y) `shouldBe` Just x
      it "returns a Position with the given y coordinate" $ property $
        forAll validCoordinates $ \(x, y) ->
          fmap getY (position x y) `shouldBe` Just y
      it "is inverted by unPosition" $ property $
        forAll validCoordinates $ \(x, y) ->
          fmap unPosition (position x y) `shouldBe` Just (x, y)
