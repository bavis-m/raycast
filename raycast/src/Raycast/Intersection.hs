module Raycast.Intersection (intersections, Ray, mkRay, rayStart, rayDelta, Intersection(intersectionCell, direction, tValue), collisionU) where

import Data.Ord
import Control.Monad.Trans.State.Lazy
import Debug.Trace

import Data.Vect.Double.Base
import Data.Vect.Double.Util.Dim2

import Util.Util
import Raycast.SimpleLens
import Raycast.Grid

-- Ray: a start position and delta, where the delta *must* be of length 1
-- mkRay returns Nothing if you pass a 0-vector
data Ray = Ray {-# UNPACK #-} !(Vec2, Vec2) deriving Show
mkRay :: Vec2 -> Vec2 -> Ray
mkRay start delta | len delta > 0.0001 = Ray (start, normalize delta)
                  | otherwise =          error "mkRay: Must pass a delta with |delta|>0"
                                        
rayStart (Ray (start, _)) = start
rayDelta (Ray (_, delta)) = delta


data IntersectionState = IST
                         {
                           istCell :: {-# UNPACK #-} !Cell,
                           istStepX :: {-# UNPACK #-} !Int,
                           istStepY :: {-# UNPACK #-} !Int,
                           istXDir :: {-# UNPACK #-} !Direction,
                           istYDir :: {-# UNPACK #-} !Direction,
                           istTMaxX :: {-# UNPACK #-} !Double,
                           istTMaxY :: {-# UNPACK #-} !Double,
                           istTDeltaX :: {-# UNPACK #-} !Double,
                           istTDeltaY :: {-# UNPACK #-} !Double
                         } deriving (Show)

step :: Double -> Int
step x | x > 0 = 1
       | x < 0 = -1
       | otherwise = 0

infinity = read "Infinity" :: Double
       
tMax :: Double -> Double -> Double
tMax s d | d == 0 = infinity
         | d > 0 = abs $ ((fromIntegral $ floor (s + 1)) - s) / d
         | d < 0 = abs $ (s - (fromIntegral $ ceiling (s - 1))) / d

dirX :: Double -> Direction
dirX d | d > 0 = posX
       | otherwise = negX

dirY :: Double -> Direction
dirY d | d > 0 = posY
       | otherwise = negY

       
initialState (Ray (s@(Vec2 startX startY), d@(Vec2 deltaX deltaY))) =
  (IST (mkCell s) (step deltaX) (step deltaY) (dirX deltaX) (dirY deltaY) (tMax startX deltaX) (tMax startY deltaY) (abs $ 1.0 / deltaX) (abs $ 1.0 / deltaY))
  

      
data Intersection = Intersection
                    {
                      intersectionCell :: {-# UNPACK #-} !Cell,
                      direction :: {-# UNPACK #-} !Direction,
                      tValue :: {-# UNPACK #-} !Double
                    } deriving (Show)

-- Compute a list of cells the given ray (start, delta) passes through
intersections :: Ray -> [Intersection]
intersections r = intersections' 0 $ initialState r
  where
    intersections' :: Double -> IntersectionState -> [Intersection]
    intersections' t state = let
        (newState, dir, newT) = advanceState state
        newCell = istCell newState
      in (Intersection newCell dir newT) : intersections' newT newState
    advanceState :: IntersectionState -> (IntersectionState, Direction, Double)
    advanceState state | istTMaxX state < istTMaxY state =
                            let
                              newCell = moveXBy (istCell state) $ istStepX state
                              dir = istXDir state
                              newT = istTMaxX state
                              newState = state { istCell = newCell, istTMaxX = newT + istTDeltaX state }
                            in (newState, dir, newT)
                       | otherwise = 
                            let
                              newCell = moveYBy (istCell state) $ istStepY state
                              dir = istYDir state
                              newT = istTMaxY state
                              newState = state { istCell = newCell, istTMaxY = newT + istTDeltaY state }
                            in (newState, dir, newT)

collisionPoint :: Ray -> Double -> Vec2
collisionPoint (Ray (start, delta)) t = start &+ delta &* t

-- u-value of a collision is the (0,1) ranged value of where the ray hit on the 'side wall'
-- of the cell it entered: this is always from left-to-right looking at the face of the wall
collisionU :: Ray -> Intersection -> Double
collisionU ray intersection | dir == posX = 1.0 - fy
                            | dir == negX = fy
                            | dir == posY = fx
                            | dir == negY = 1.0 - fx
  where dir = direction intersection
        Vec2 finalX finalY = collisionPoint ray (tValue intersection)
        fx = positiveFractionOf finalX
        fy = positiveFractionOf finalY