module Position.OffsetSpec (spec) where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import Expectation
import Position
import Position.Offset
import PositionArbitraries

spec :: Spec
spec = do
  describe "horizontal" $ do
    it "is inverted by a negated horizontal offset (when defined)" $ property $ \d p ->
      (horizontal d p >>= horizontal (-d)) `shouldBeMaybe` p
    it "is commutative with vertical offsets (when defined)" $ property $ \d d' p ->
      (horizontal d p >>= vertical d') `shouldBeMaybe2` (vertical d' p >>= horizontal d)
    it "distributes over addition (when defined)" $ property $ \d d' p ->
      horizontal (d + d') p `shouldBeMaybe2` (horizontal d p >>= horizontal d')
    context "given a zero offset" $ do
      it "returns the same position" $ property $ \p ->
        horizontal 0 p `shouldBe` Just p
    context "given a non-zero offset" $ do
      it "returns a different position" $ property $ \d p ->
        d /= 0 ==> horizontal d p `shouldNotBe` Just p
      it "is not inverted by a non-zero vertical offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (horizontal d p >>= vertical d') `shouldNotBe` Just p
      it "is not inverted by a non-zero main diagonal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (horizontal d p >>= mainDiagonal d') `shouldNotBe` Just p
      it "is not inverted by a non-zero anti diagonal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (horizontal d p >>= antiDiagonal d') `shouldNotBe` Just p
  describe "vertical" $ do
    it "is inverted by a negated vertical offset (when defined)" $ property $ \d p ->
      (vertical d p >>= vertical (-d)) `shouldBeMaybe` p
    it "is commutative with horizontal offsets (when defined)" $ property $ \d d' p ->
      (vertical d p >>= horizontal d') `shouldBeMaybe2` (horizontal d' p >>= vertical d)
    it "distributes over addition (when defined)" $ property $ \d d' p ->
      vertical (d + d') p `shouldBeMaybe2` (vertical d p >>= vertical d')
    context "given a zero offset" $ do
      it "returns the same position" $ property $ \p ->
        vertical 0 p `shouldBe` Just p
    context "given a non-zero offset" $ do
      it "returns a different position" $ property $ \d p ->
        d /= 0 ==> vertical d p `shouldNotBe` Just p
      it "is not inverted by a non-zero horizontal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (vertical d p >>= horizontal d') `shouldNotBe` Just p
      it "is not inverted by a non-zero main diagonal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (vertical d p >>= mainDiagonal d') `shouldNotBe` Just p
      it "is not inverted by a non-zero anti diagonal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (vertical d p >>= antiDiagonal d') `shouldNotBe` Just p
  describe "mainDiagonal" $ do
    it "is inverted by a negated main diagonal offset (when defined)" $ property $ \d p ->
      (mainDiagonal d p >>= mainDiagonal (-d)) `shouldBeMaybe` p
    it "is commutative with anti diagonal offsets (when defined)" $ property $ \d d' p ->
      (mainDiagonal d p >>= antiDiagonal d') `shouldBeMaybe2` (antiDiagonal d' p >>= mainDiagonal d)
    it "distributes over addition (when defined)" $ property $ \d d' p ->
      mainDiagonal (d + d') p `shouldBeMaybe2` (mainDiagonal d p >>= mainDiagonal d')
    context "given a zero offset" $ do
      it "returns the same position" $ property $ \p ->
        mainDiagonal 0 p `shouldBe` Just p
    context "given a non-zero offset" $ do
      it "returns a different position" $ property $ \d p ->
        d /= 0 ==> mainDiagonal d p `shouldNotBe` Just p
      it "is not inverted by a non-zero anti diagonal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (mainDiagonal d p >>= antiDiagonal d') `shouldNotBe` Just p
      it "is not inverted by a non-zero horizontal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (mainDiagonal d p >>= horizontal d') `shouldNotBe` Just p
      it "is not inverted by a non-zero vertical offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (mainDiagonal d p >>= vertical d') `shouldNotBe` Just p
  describe "antiDiagonal" $ do
    it "is inverted by a negated anti diagonal offset (when defined)" $ property $ \d p ->
      (antiDiagonal d p >>= antiDiagonal (-d)) `shouldBeMaybe` p
    it "is commutative with main diagonal offsets (when defined)" $ property $ \d d' p ->
      (antiDiagonal d p >>= mainDiagonal d') `shouldBeMaybe2` (mainDiagonal d' p >>= antiDiagonal d)
    it "distributes over addition (when defined)" $ property $ \d d' p ->
      antiDiagonal (d + d') p `shouldBeMaybe2` (antiDiagonal d p >>= antiDiagonal d')
    context "given a zero offset" $ do
      it "returns the same position" $ property $ \p ->
        antiDiagonal 0 p `shouldBe` Just p
    context "given a non-zero offset" $ do
      it "returns a different position" $ property $ \d p ->
        d /= 0 ==> antiDiagonal d p `shouldNotBe` Just p
      it "is not inverted by a non-zero main diagonal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (antiDiagonal d p >>= mainDiagonal d') `shouldNotBe` Just p
      it "is not inverted by a non-zero horizontal offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (antiDiagonal d p >>= horizontal d') `shouldNotBe` Just p
      it "is not inverted by a non-zero vertical offset" $ property $ \d d' p ->
        d /= 0 && d' /= 0 ==> (antiDiagonal d p >>= vertical d') `shouldNotBe` Just p
