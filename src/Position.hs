module Position
  ( Position ()
  , getX
  , getY
  , isValid
  , position
  , unPosition
  ) where

data Position = Position Int Int
  deriving (Eq, Ord, Show)

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
