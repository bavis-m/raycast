module Game.Level (Level, mkLevel, tile) where

import Data.Array.IArray
import Raycast.Grid

data Level a = Level { levelWidth :: Int, levelHeight :: Int, defaultTile :: a, tiles :: Array Int a }

mkLevel :: Int -> Int -> a -> [a] -> Level a
mkLevel w h defaultTile listTiles | length listTiles == w * h = Level w h defaultTile $ listArray (0, w * h - 1) listTiles
                                  | otherwise = error "Invalid level size"

tile :: Level a -> Cell -> a
tile level cell | x >= 0 && x < levelWidth level && y >= 0 && y < levelHeight level = tiles level ! (y * levelWidth level + x)
                | otherwise = defaultTile level
  where
    x = cellX cell
    y = cellY cell