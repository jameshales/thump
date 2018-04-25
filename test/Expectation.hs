module Expectation where

import Data.Maybe
import Test.Hspec

infix 1 `shouldBeMaybe`

expectationSuccess :: Expectation
expectationSuccess = return ()

shouldBeMaybe :: (Eq a, Show a) => Maybe a -> a -> Expectation
shouldBeMaybe ma b = maybe expectationSuccess (`shouldBe` b) ma

shouldBeMaybe2 :: (Eq a, Show a) => Maybe a -> Maybe a -> Expectation
shouldBeMaybe2 ma mb = fromMaybe expectationSuccess $ shouldBe <$> ma <*> mb
