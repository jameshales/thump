module PositionArbitraries where

import Data.Maybe
import Test.Hspec
import Test.QuickCheck

import Position

boundedCoordinate :: Gen Int
boundedCoordinate = choose (0, 14)

boundedCoordinates :: Gen (Int, Int)
boundedCoordinates = (,) <$> boundedCoordinate <*> boundedCoordinate

unboundedCoordinate :: Gen Int
unboundedCoordinate = oneof [choose (minBound, -1), choose(15, maxBound)]

unboundedCoordinates :: Gen (Int, Int)
unboundedCoordinates = (,) <$> unboundedCoordinate <*> unboundedCoordinate

validCoordinates :: Gen (Int, Int)
validCoordinates = boundedCoordinates `suchThat` (isJust . uncurry position)

invalidCoordinates :: Gen (Int, Int)
invalidCoordinates = oneof
  [ boundedCoordinates `suchThat` (not . uncurry isValid)
  , unboundedCoordinates
  ]

unsafePosition :: Int -> Int -> Position
unsafePosition = (maybe (error "Position.arbitrary") id .) . position

instance Arbitrary Position where
  arbitrary = fmap (uncurry unsafePosition) validCoordinates
