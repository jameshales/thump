module Piece
  ( Piece (..)
  ) where

data Piece = Dwarf | Stone | Troll
  deriving (Eq, Ord, Show)
