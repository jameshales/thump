module BoardSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Board
import Piece
import Position
import PositionArbitraries

spec :: Spec
spec = do
  describe "empty" $ do
    it "contains 0 pieces" $ do
      (length $ pieces empty) `shouldBe` 0
  describe "initial" $ do
    it "contains 32 dwarves" $ do
      (length $ filter (== Dwarf) $ pieces initial) `shouldBe` 32
    it "contains 8 trolls" $ do
      (length $ filter (== Troll) $ pieces initial) `shouldBe` 8
    it "contains 1 stone" $ do
      (length $ filter (== Stone) $ pieces initial) `shouldBe` 1
