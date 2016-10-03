module Raycast.Grid (Cell, mkCell, cellX, cellY, Direction, posX, negX, posY, negY, moveXBy, moveYBy) where

import Data.Vect.Double.Base

-- Cell
-- Represents a cell on the grid
data Cell = Cell {-# UNPACK #-} !(Int, Int)
instance Show Cell where
  show (Cell t) = show t

cellX :: Cell -> Int
cellX (Cell (x, _)) = x
cellY :: Cell -> Int
cellY (Cell (_, y)) = y

moveXBy :: Cell -> Int -> Cell
moveXBy (Cell (x, y)) s = Cell (x + s, y)
moveYBy :: Cell -> Int -> Cell
moveYBy (Cell (x, y)) s = Cell (x, y + s)

-- get the cell a point is in
mkCell :: Vec2 -> Cell
mkCell (Vec2 x y) = Cell (floor x, floor y)

newtype Direction = Direction Int deriving (Eq)

instance Show Direction where
  show (Direction 0) = "PosX"
  show (Direction 1) = "NegX"
  show (Direction 2) = "PosY"
  show (Direction 3) = "NegY"

posX :: Direction
posX = Direction 0

negX :: Direction
negX = Direction 1

posY :: Direction
posY = Direction 2

negY :: Direction
negY = Direction 3