module Position.Comparison
  ( isHorizontal
  , isVertical
  , isOrthogonal
  , isMainDiagonal
  , isAntiDiagonal
  , isDiagonal
  , isAligned
  , distance
  , Direction (..)
  ) where

import Position

data Direction =
    AboveOf
  | BelowOf
  | LeftOf
  | RightOf
  | AboveLeftOf
  | AboveRightOf
  | BelowLeftOf
  | BelowRightOf
  deriving (Eq, Ord, Show)

isHorizontal :: Position -> Position -> Bool
isHorizontal p q = getY p == getY q

isLeftOf :: Position -> Position -> Bool
isLeftOf p q = isHorizontal p q && getX p < getX q

isRightOf :: Position -> Position -> Bool
isRightOf p q = isHorizontal p q && getX p > getX q

isVertical :: Position -> Position -> Bool
isVertical p q = getX p == getX q

isAboveOf :: Position -> Position -> Bool
isAboveOf p q = isVertical p q && getY p < getY q

isBelowOf :: Position -> Position -> Bool
isBelowOf p q = isVertical p q && getY p > getY q

isMainDiagonal :: Position -> Position -> Bool
isMainDiagonal p q = (getX p - getX q) == (getY p - getY q)

isAboveLeftOf :: Position -> Position -> Bool
isAboveLeftOf p q = isMainDiagonal p q && getX p < getX q

isBelowRightOf :: Position -> Position -> Bool
isBelowRightOf p q = isMainDiagonal p q && getX p > getX q

isAntiDiagonal :: Position -> Position -> Bool
isAntiDiagonal p q = (getX q - getX p) == (getY p - getY q)

isBelowLeftOf :: Position -> Position -> Bool
isBelowLeftOf p q = isAntiDiagonal p q && getX p < getX q

isAboveRightOf :: Position -> Position -> Bool
isAboveRightOf p q = isAntiDiagonal p q && getX p > getX q

isOrthogonal :: Position -> Position -> Bool
isOrthogonal p q = isHorizontal p q || isVertical p q

isDiagonal :: Position -> Position -> Bool
isDiagonal p q = isMainDiagonal p q || isAntiDiagonal p q

isAligned :: Position -> Position -> Bool
isAligned p q = isOrthogonal p q || isDiagonal p q

distance :: Position -> Position -> Int
distance p q = max (abs $ getX p - getX q) (abs $ getY p - getY q)

reverse :: Direction -> Direction
reverse d = case d of
  LeftOf       -> RightOf
  RightOf      -> LeftOf
  AboveOf      -> BelowOf
  BelowOf      -> AboveOf
  AboveLeftOf  -> BelowRightOf
  AboveRightOf -> BelowLeftOf
  BelowLeftOf  -> AboveRightOf
  BelowRightOf -> AboveLeftOf

direction :: Position -> Position -> Maybe Direction
direction p q | p `isLeftOf`       q = Just LeftOf
              | p `isRightOf`      q = Just RightOf
              | p `isAboveOf`      q = Just AboveOf
              | p `isBelowOf`      q = Just BelowOf
              | p `isAboveLeftOf`  q = Just AboveLeftOf
              | p `isAboveRightOf` q = Just AboveRightOf
              | p `isBelowLeftOf`  q = Just BelowLeftOf
              | p `isBelowRightOf` q = Just BelowRightOf
              | otherwise            = Nothing
