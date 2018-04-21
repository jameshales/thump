module Position.OffsetSpec (spec) where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import Position
import Position.Offset
import PositionArbitraries

expectationSuccess :: Expectation
expectationSuccess = return ()

spec :: Spec
spec = do
  describe "horizontal" $ do
    it "is inverted by a negated horizontal offset (when defined)" $ property $ \d p ->
      maybe expectationSuccess (`shouldBe` p) (horizontal d p >>= horizontal (-d))
    it "is commutative with vertical offsets (when defined)" $ property $ \d d' p ->
      maybe expectationSuccess (uncurry shouldBe) ((,) <$> (horizontal d p >>= vertical d') <*> (vertical d' p >>= horizontal d))
    it "distributes over addition (when defined)" $ property $ \d d' p ->
      maybe expectationSuccess (uncurry shouldBe) ((,) <$> horizontal (d + d') p <*> (horizontal d p >>= horizontal d'))
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
      maybe expectationSuccess (`shouldBe` p) (vertical d p >>= vertical (-d))
    it "is commutative with horizontal offsets (when defined)" $ property $ \d d' p ->
      maybe expectationSuccess (uncurry shouldBe) ((,) <$> (vertical d p >>= horizontal d') <*> (horizontal d' p >>= vertical d))
    it "distributes over addition (when defined)" $ property $ \d d' p ->
      maybe expectationSuccess (uncurry shouldBe) ((,) <$> vertical (d + d') p <*> (vertical d p >>= vertical d'))
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
      maybe expectationSuccess (`shouldBe` p) (mainDiagonal d p >>= mainDiagonal (-d))
    it "is commutative with anti diagonal offsets (when defined)" $ property $ \d d' p ->
      maybe expectationSuccess (uncurry shouldBe) ((,) <$> (mainDiagonal d p >>= antiDiagonal d') <*> (antiDiagonal d' p >>= mainDiagonal d))
    it "distributes over addition (when defined)" $ property $ \d d' p ->
      maybe expectationSuccess (uncurry shouldBe) ((,) <$> mainDiagonal (d + d') p <*> (mainDiagonal d p >>= mainDiagonal d'))
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
      maybe expectationSuccess (`shouldBe` p) (antiDiagonal d p >>= antiDiagonal (-d))
    it "is commutative with main diagonal offsets (when defined)" $ property $ \d d' p ->
      maybe expectationSuccess (uncurry shouldBe) ((,) <$> (antiDiagonal d p >>= mainDiagonal d') <*> (mainDiagonal d' p >>= antiDiagonal d))
    it "distributes over addition (when defined)" $ property $ \d d' p ->
      maybe expectationSuccess (uncurry shouldBe) ((,) <$> antiDiagonal (d + d') p <*> (antiDiagonal d p >>= antiDiagonal d'))
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
