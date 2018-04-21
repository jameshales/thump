module PositionSpec (spec) where

import Control.Applicative
import Data.Ix
import Data.List
import Data.Maybe
import Data.Ord
import Test.Hspec
import Test.QuickCheck

import Position
import PositionArbitraries

spec :: Spec
spec = do
  describe "position" $ do
    it "inverts unPosition" $ property $ \p ->
      uncurry position (unPosition p) `shouldBe` Just p
    context "given (0, 0)" $ do
      it "returns nothing" $ do
        position 0 0 `shouldBe` Nothing
    context "given (4, 0)" $ do
      it "returns nothing" $ do
        position 4 0 `shouldBe` Nothing
    context "given (5, 0)" $ do
      it "returns a position" $ do
        position 5 0 `shouldSatisfy` isJust
    context "given (9, 0)" $ do
      it "returns a position" $ do
        position 9 0 `shouldSatisfy` isJust
    context "given (10, 0)" $ do
      it "returns nothing" $ do
        position 10 0 `shouldBe` Nothing
    context "given (14, 0)" $ do
      it "returns nothing" $ do
        position 14 0 `shouldBe` Nothing
    context "given (0, 14)" $ do
      it "returns nothing" $ do
        position 0 14 `shouldBe` Nothing
    context "given (4, 14)" $ do
      it "returns nothing" $ do
        position 4 14 `shouldBe` Nothing
    context "given (5, 14)" $ do
      it "returns a position" $ do
        position 5 14 `shouldSatisfy` isJust
    context "given (9, 14)" $ do
      it "returns a position" $ do
        position 9 14 `shouldSatisfy` isJust
    context "given (10, 14)" $ do
      it "returns nothing" $ do
        position 10 14 `shouldBe` Nothing
    context "given (14, 14)" $ do
      it "returns nothing" $ do
        position 14 14 `shouldBe` Nothing
    context "given (0, 0)" $ do
      it "returns nothing" $ do
        position 0 0 `shouldBe` Nothing
    context "given (0, 4)" $ do
      it "returns nothing" $ do
        position 0 4 `shouldBe` Nothing
    context "given (0, 5)" $ do
      it "returns a position" $ do
        position 0 5 `shouldSatisfy` isJust
    context "given (0, 9)" $ do
      it "returns a position" $ do
        position 0 9 `shouldSatisfy` isJust
    context "given (0, 10)" $ do
      it "returns nothing" $ do
        position 0 10 `shouldBe` Nothing
    context "given (0, 14)" $ do
      it "returns nothing" $ do
        position 0 14 `shouldBe` Nothing
    context "given (14, 0)" $ do
      it "returns nothing" $ do
        position 14 0 `shouldBe` Nothing
    context "given (14, 4)" $ do
      it "returns nothing" $ do
        position 14 4 `shouldBe` Nothing
    context "given (14, 5)" $ do
      it "returns a position" $ do
        position 14 5 `shouldSatisfy` isJust
    context "given (14, 9)" $ do
      it "returns a position" $ do
        position 14 9 `shouldSatisfy` isJust
    context "given (14, 10)" $ do
      it "returns nothing" $ do
        position 14 10 `shouldBe` Nothing
    context "given (14, 14)" $ do
      it "returns nothing" $ do
        position 14 14 `shouldBe` Nothing
    context "given a x < 0" $ do
      it "returns nothing" $ property $ \y ->
        forAll (choose (minBound, -1)) $ \x ->
          position x y `shouldBe` Nothing
    context "given x > 14" $ do
      it "returns nothing" $ property $ \y ->
        forAll (choose (15, maxBound)) $ \x ->
          position x y `shouldBe` Nothing
    context "given y < 0" $ do
      it "returns nothing" $ property $ \x ->
        forAll (choose (minBound, -1)) $ \y ->
          position x y `shouldBe` Nothing
    context "given y > 14" $ do
      it "returns nothing" $ property $ \x ->
        forAll (choose (15, maxBound)) $ \y ->
          position x y `shouldBe` Nothing
    context "given 0 <= x <= 14 and 5 <= y <= 9" $ do
      it "returns a position" $ property $
        forAll (choose (0, 14)) $ \x ->
          forAll (choose (5, 9)) $ \y ->
            position x y `shouldSatisfy` isJust
    context "given 5 <= x <= 9 and 0 <= y <= 14" $ do
      it "returns a position" $ property $
        forAll (choose (5, 9)) $ \x ->
          forAll (choose (0, 14)) $ \y ->
            position x y `shouldSatisfy` isJust
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
  describe "instance Bounded Position" $ do
    it "minBound is less than or equal to every position" $ property $ \p ->
      (minBound :: Position) `shouldSatisfy` (<= p)
    it "maxBound is greater than or equal to every position" $ property $ \p ->
      (maxBound :: Position) `shouldSatisfy` (>= p)
  describe "instance Ord Position" $ do
    it "p == p" $ property $ \p ->
      compare (p :: Position) p `shouldBe` EQ
    it "p == q iff q == p" $ property $ \p q ->
      compare (p :: Position) q == EQ `shouldBe` compare q p == EQ
    it "p == q && q == r iff p ==> r" $ property $ \p q r ->
      not (compare (p :: Position) q == EQ && compare q r == EQ) || compare p r == EQ `shouldBe` True
    it "p < q iff q > p" $ property $ \p q ->
      compare (p :: Position) q == LT `shouldBe` compare (q :: Position) p == GT
    it "p <= q iff q >= p" $ property $ \p q ->
      compare (p :: Position) q /= GT `shouldBe` compare (q :: Position) p /= LT
    it "p < q && q < r ==> p < r" $ property $ \p q r ->
      not (compare (p :: Position) q == LT && compare q r == LT) || compare p r == LT `shouldBe` True
    it "p <= q && q <= r ==> p <= r" $ property $ \p q r ->
      not (compare (p :: Position) q /= GT && compare q r /= GT) || compare p r /= GT `shouldBe` True
  describe "instance Ix Position" $ do
    it "inRange (l, u) i == elem i (range (l, u))" $ property $ \l u i ->
      l <= u ==> inRange (l :: Position, u) i `shouldBe` elem i (range (l, u))
    it "range (l, u) !! index (l, u) i == i, when inRange (l,u) i" $ property $ \l u i ->
      l <= u && inRange (l, u) i ==> range (l :: Position, u) !! index (l, u) i `shouldBe` i
    it "map (index (l, u)) (range (l, u)) == [0..rangeSize (l, u) - 1]" $ property $ \l u ->
      l <= u ==> map (index (l :: Position, u)) (range (l, u)) `shouldBe` [0..rangeSize (l, u) - 1]
    it "rangeSize (l, u) == length (range (l, u))" $ property $ \l u ->
      l <= u ==> rangeSize (l :: Position, u) `shouldBe` length (range (l, u))
    it "index returns the numbers [0..164]" $ do
      map (index (minBound, maxBound)) positions `shouldBe` [0..164]
  describe "positions" $ do
    it "is a list of all positions" $ do
      positions `shouldBe` mapMaybe (uncurry position) [(x, y) | y <- [0..14], x <- [0..14]]
    it "is a sorted list" $ do
      sort positions `shouldBe` positions
