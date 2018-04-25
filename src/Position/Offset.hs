module Position.Offset
  ( horizontal
  , vertical
  , mainDiagonal
  , antiDiagonal
  , offset
  , offsets
  ) where

import Data.Maybe
import Position
import Position.Comparison

horizontal :: Int -> Position -> Maybe Position
horizontal d p = position (getX p + d) (getY p)

vertical :: Int -> Position -> Maybe Position
vertical d p = position (getX p) (getY p + d)

mainDiagonal :: Int -> Position -> Maybe Position
mainDiagonal d p = position (getX p + d) (getY p + d)

antiDiagonal :: Int -> Position -> Maybe Position
antiDiagonal d p = position (getX p + d) (getY p - d)

offset :: Direction -> Int -> Position -> Maybe Position
offset d d' p = case d of
  LeftOf       -> horizontal (-d') p
  RightOf      -> horizontal d' p
  AboveOf      -> vertical (-d') p
  BelowOf      -> vertical d' p
  AboveLeftOf  -> mainDiagonal (-d') p
  AboveRightOf -> antiDiagonal (-d') p
  BelowLeftOf  -> antiDiagonal d' p
  BelowRightOf -> mainDiagonal d' p

offsets :: Direction -> Int -> Position -> [Position]
offsets d d' p = mapMaybe (\d'' -> offset d d'' p) [0..d']
