module Board
  ( Board ()
  , empty
  , initial
  , getPiece
  , pieces
  , pieceAssocs
  ) where

import Data.Array
import Data.Maybe

import Piece
import Position
import Position.Comparison

newtype Board = MkBoard (Array Position (Maybe Piece))

board :: [(Position, Maybe Piece)] -> Board
board = MkBoard . array (minBound, maxBound)

unBoard :: Board -> Array Position (Maybe Piece)
unBoard (MkBoard a) = a

isBoundary :: Position -> Bool
isBoundary p = x == 0      -- Left
            || x == 14     -- Right
            || y == 0      -- Top
            || y == 14     -- Bottom 
            || x + y == 5  -- Top-left
            || x - y == 9  -- Top-right
            || y - x == 9  -- Bottom-left
            || x + y == 23 -- Bottom-right
  where (x, y) = unPosition p

initialPiece :: Position -> Maybe Piece
initialPiece p | x == 7 && y == 7 = Just Stone
               | 6 <= x && x <= 8
              && 6 <= y && y <= 8 = Just Troll
               | isBoundary p     
              && x /= 7 && y /= 7 = Just Dwarf
               | otherwise        = Nothing
  where (x, y) = unPosition p

empty :: Board
empty = board [(p, Nothing) | p <- positions]

initial :: Board
initial = board [(p, initialPiece p) | p <- positions]

pieces :: Board -> [Piece]
pieces = catMaybes . elems . unBoard

pieceAssocs :: Board -> [(Position, Piece)]
pieceAssocs = mapMaybe (\(c, p) -> fmap ((,) c) p) . assocs . unBoard

getPiece :: Board -> Position -> Maybe Piece
getPiece b p = unBoard b ! p

occupied :: Board -> Position -> Bool
occupied = (isJust .) . getPiece

vacant :: Board -> Position -> Bool
vacant = (not .) . vacant

isValidDwarfMove :: Position -> Position -> Bool
isValidDwarfMove p q = d' >= 1 && maybe False (\d -> 
  where md = direction p q
        d' = distance p q

isValidTrollMove :: Position -> Position -> Bool
isValidTrollMove p q = distance p q == 1

data MoveError = SourceVacant | DestinationOccupied | InvalidMove
  deriving (Eq, Ord, Show)

movePiece :: Board -> Position -> Position -> Either MoveError Board
movePiece b p q | occupied b q       = Left DestinationOccupied
                | otherwise          = Right $ MkBoard $ unBoard b // [(p, Nothing), (q, getPiece b p)]
