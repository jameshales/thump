module Position.Offset
  ( horizontal
  , vertical
  , mainDiagonal
  , antiDiagonal
  ) where

import Position

horizontal :: Int -> Position -> Maybe Position
horizontal d p = position (getX p + d) (getY p)

vertical :: Int -> Position -> Maybe Position
vertical d p = position (getX p) (getY p + d)

mainDiagonal :: Int -> Position -> Maybe Position
mainDiagonal d p = position (getX p + d) (getY p + d)

antiDiagonal :: Int -> Position -> Maybe Position
antiDiagonal d p = position (getX p + d) (getY p - d)
