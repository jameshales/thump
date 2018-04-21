module Position.Comparison
  ( isHorizontal
  , isVertical
  , isOrthogonal
  , isMainDiagonal
  , isAntiDiagonal
  , isDiagonal
  , isAligned
  ) where

import Position

isHorizontal :: Position -> Position -> Bool
isHorizontal p q = getY p == getY q

isVertical :: Position -> Position -> Bool
isVertical p q = getX p == getX q

isMainDiagonal :: Position -> Position -> Bool
isMainDiagonal p q = (getX p - getX q) == (getY p - getY q)

isAntiDiagonal :: Position -> Position -> Bool
isAntiDiagonal p q = (getX q - getX p) == (getY p - getY q)

isOrthogonal :: Position -> Position -> Bool
isOrthogonal p q = isHorizontal p q || isVertical p q

isDiagonal :: Position -> Position -> Bool
isDiagonal p q = isMainDiagonal p q || isAntiDiagonal p q

isAligned :: Position -> Position -> Bool
isAligned p q = isOrthogonal p q || isDiagonal p q
