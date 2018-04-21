module Position
  ( Position ()
  , getX
  , getY
  , isValid
  , position
  , unPosition
  , positions
  ) where

import Data.Ix
import Data.List
import Data.Maybe
import Data.Ord

data Position = Position Int Int
  deriving (Eq, Show)

instance Bounded Position where
  minBound = Position 5 0
  maxBound = Position 9 14

instance Ord Position where
  compare p q = compare (y, x) (y', x')
    where (x, y)   = unPosition p
          (x', y') = unPosition q

index' :: Position -> Int
index' p | y < 5     = y ^ 2 + 5 * y + x - 5
         | y < 10    = 15 * y + x - 30
         | otherwise = - y ^ 2 + 33 * y - 111 + x
  where (x, y) = unPosition p

succ' :: Position -> Maybe Position
succ' p | x < 14 && x - y < 9 && x + y < 23 = Just $ Position (x + 1) y
        | y < 4                             = Just $ Position (4 - y) (y + 1)
        | y < 9                             = Just $ Position 0 (y + 1)
        | y < 14                            = Just $ Position (y - 8) (y + 1)
        | otherwise                         = Nothing
  where (x, y) = unPosition p

instance Ix Position where
  range (l, u) | l <= u    = l : (unfoldr (\p -> fmap (\q -> (q, q)) $ if p /= u then succ' p else Nothing) $ l)
               | otherwise = error "Position.range"

  index (l, u) p | l <= u    = index' p - index' l
                 | otherwise = error "Position.index"

  inRange (l, u) p | l <= u    = index' l <= i && i <= index' u
                   | otherwise = error "Position.inRange"
    where i = index' p

isValid :: Int -> Int -> Bool
isValid x y = x >= 0      -- Left
           && x <= 14     -- Right
           && y >= 0      -- Top
           && y <= 14     -- Bottom 
           && x + y >= 5  -- Top-left
           && x - y <= 9  -- Top-right
           && y - x <= 9  -- Bottom-left
           && x + y <= 23 -- Bottom-right

position :: Int -> Int -> Maybe Position
position x y | isValid x y = Just $ Position x y
             | otherwise   = Nothing

getX :: Position -> Int
getX (Position x _) = x 

getY :: Position -> Int
getY (Position _ y) = y

unPosition :: Position -> (Int, Int)
unPosition (Position x y) = (x, y)

positions :: [Position]
positions = range (minBound, maxBound)
