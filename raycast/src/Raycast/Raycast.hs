module Raycast.Raycast(renderColumn, ViewParams(..), Tile(..), HalfTile(..), FloorTex(..), WallTex(..), FloorUV(..), WallUV(..)) where

import qualified Data.DList as DL
import Data.Vect.Double.Base
import Raycast.Intersection
import Raycast.Grid
import Engine.RuntimeInterface
import Data.Maybe
import Debug.Trace
import Data.Tuple (swap)

data FloorUV = FloorFixed | FloorSky deriving (Show, Eq)
data WallUV = WallFixed | WallSky deriving (Show, Eq)

data FloorTex = FloorTex Int FloorUV deriving (Show, Eq)
data WallTex = WallTex Int WallUV deriving (Show, Eq)

data HalfTile = None | Floor (Double, Maybe Double) FloorTex WallTex deriving (Show)
data Tile = Wall WallTex | Open HalfTile HalfTile deriving (Show)

data RenderSurfaceData = WallType Double WallTex | FloorType Double FloorTex
data RenderData = RenderData Int Ray Intersection RenderSurfaceData

halfTileMatches :: HalfTile -> HalfTile -> Bool
halfTileMatches (Floor h1 f1 _) (Floor h2 f2 _) = (h1 == h2) && (f1 == f2)
halfTileMatches _ _ = False

tileMatches :: Tile -> Tile -> Bool
tileMatches (Open b1 t1) (Open b2 t2) = halfTileMatches b1 b2 && halfTileMatches t1 t2
tileMatches _ _ = False

-- fast?
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just a) = Just $ f a
maybeMap f Nothing = Nothing

newtype Range = Range (Int, Int)

rangeSize :: Range -> Int
rangeSize (Range (b, t)) = (t - b) + 1

rangeValid :: Range -> Bool
rangeValid = (> 0) . rangeSize

rangeBottom :: Range -> Int
rangeBottom (Range (b, _)) = b

rangeTop :: Range -> Int
rangeTop (Range (_, t)) = t

invalidRange :: Range
invalidRange = Range (1, 0)

data ChunkSide = Bottom | Top

chunkWindow :: Range -> ChunkSide -> Int -> (Range, Range)
chunkWindow (Range (windowB, windowT)) Bottom v = (Range (max windowB (v + 1), windowT), Range (windowB, min v windowT))
chunkWindow (Range (windowB, windowT)) Top v = (Range (windowB, min windowT (v - 1)), Range (max windowB v, windowT))

-- splits something off of the bottom of the range (original range in fst)
splitRange :: Range -> ChunkSide -> Maybe Int -> (Range, DL.DList Range)
splitRange r _ Nothing = (r, DL.empty)
splitRange (Range (rB, rT)) Bottom (Just s) = (Range (s + 1, rT), DL.singleton $ Range (rB, s))
splitRange (Range (rB, rT)) Top (Just s) = (Range (rB, s - 1), DL.singleton $ Range (s, rT))

data ViewParams = ViewParams { vpBottomSkyTex :: Int, vpTopSkyTex :: Int, vpHorizonY :: Double, vpHeightScale :: Double, vpTileRenderDist :: Int, vpEyeHeight :: Double }

floorChunkWorldSpaceStart :: Double
floorChunkWorldSpaceStart = 0.15

floorChunkWorldSpaceIncrease :: Double
floorChunkWorldSpaceIncrease = 0.03

heightDistanceY :: ViewParams -> Double -> Double -> Int
heightDistanceY (ViewParams _ _ horizonY heightScale _ eyeHeight) distanceInv height = round ((height - eyeHeight) * distanceInv * heightScale + horizonY)

yDistanceHeight :: ViewParams -> Int -> Double -> Double
yDistanceHeight (ViewParams _ _ horizonY heightScale _ eyeHeight) y distance = eyeHeight + distance * ((fromIntegral y - horizonY) / heightScale)

yHeightDistance :: ViewParams -> Int -> Double -> Double
yHeightDistance (ViewParams _ _ horizonY heightScale _ eyeHeight) y height = (height - eyeHeight) / ((fromIntegral y - horizonY) / heightScale)

type ProduceFromRange = Range -> (Range, DL.DList (Range, RenderData))

thenSections :: ProduceFromRange -> ProduceFromRange -> ProduceFromRange
thenSections f1 f2 r =
  let
    (window, rs) = f1 r
    (window', rs') = f2 window
  in (window', rs `DL.append` rs')
infixl 7 `thenSections`

renderColumn :: RenderParams -> (Cell -> Tile) -> Int -> Ray -> Vec2 -> ViewParams -> [ColumnSection]
renderColumn params getTile column ray forward viewParams = let
    startCell = mkCell . rayStart $ ray
    startTile = getTile $ startCell
    all = take (vpTileRenderDist viewParams) $ intersections ray
    distScale = rayDelta  ray &. normalize forward
    
    -- take the list of intersections, and pick out only the ones where the column of pixels changes
    collapseIntersections :: Tile -> [Intersection] -> [Intersection]
    collapseIntersections t [] = []
    collapseIntersections t (c:[]) = [c]
    collapseIntersections t (c:cs) | tileMatches t nt = more
                                   | otherwise = c : more
        where nt = getTile . intersectionCell $ c
              more = collapseIntersections nt cs
    
    -- take a range of pixels on the floor, and split them up so we can z-interpolate the ends to make it look decent
    splitFloorRange :: ChunkSide -> Double -> Range -> Double -> DL.DList Range
    splitFloorRange Bottom height r@(Range (start, end)) chunkDistWorldSpace
          | (end - start) + 1 <= maxSize = DL.singleton r
          | otherwise = Range (start, start + maxSize - 1) `DL.cons` splitFloorRange Bottom height (Range (start + maxSize, end)) nextChunkDistWorldSpace
        where startDist = yHeightDistance viewParams start height
              nextChunkDistWorldSpace = chunkDistWorldSpace + floorChunkWorldSpaceIncrease
              maxSize = max 5 $ heightDistanceY viewParams (1.0 / (startDist + chunkDistWorldSpace)) height - start
    splitFloorRange Top height r@(Range (start, end)) chunkDistWorldSpace
          | (end - start) + 1 <= maxSize = DL.singleton r
          | otherwise = Range (end - maxSize + 1, end) `DL.cons` splitFloorRange Top height (Range (start, end - maxSize)) nextChunkDistWorldSpace
        where endDist = yHeightDistance viewParams end height
              nextChunkDistWorldSpace = chunkDistWorldSpace + floorChunkWorldSpaceIncrease
              maxSize = max 5 $ end - heightDistanceY viewParams (1.0 / (endDist + chunkDistWorldSpace)) height

    -- calculate the UV of a floor point along the ray
    floorUV :: FloorUV -> Double -> Int -> Vec2
    floorUV FloorFixed height y = rayStart ray &+ (rayDelta ray &* (yHeightDistance viewParams y height / distScale))    
    floorUV FloorSky height y = rayDelta ray &* (yHeightDistance viewParams y 0 / distScale)
    
    wallUV :: WallUV -> Ray -> Intersection -> Double -> Vec2
    wallUV WallFixed ray i height = Vec2 (collisionU ray i) height
    wallUV WallSky ray i height = rayDelta ray &* (tValue i / distScale)
    
    sectionFromRange :: (Range, RenderData) -> ColumnSection
    sectionFromRange (Range (bottom, top), RenderData column ray i (WallType distance (WallTex tex uv))) =
      ColumnSection column bottom top tex uStart vStart uEnd vEnd
        where heightBottom = yDistanceHeight viewParams bottom distance
              heightTop = yDistanceHeight viewParams top distance
              (Vec2 uStart vStart) = wallUV uv ray i heightBottom
              (Vec2 uEnd vEnd) = wallUV uv ray i heightTop
    sectionFromRange (Range (bottom, top), RenderData column ray i (FloorType h (FloorTex tex uv))) =
      ColumnSection column bottom top tex floorStartU floorStartV floorEndU floorEndV
        where (Vec2 floorStartU floorStartV) = floorUV uv h bottom
              (Vec2 floorEndU floorEndV) = floorUV uv h top
    
    -- find all the column sections for this column
    findRanges :: Tile -> Range -> [Intersection] -> DL.DList (Range, RenderData)
    findRanges currentTile window [] = DL.empty
    findRanges currentTile window (i:is) =
        let
          distance = tValue i * distScale
          distanceInv = 1.0 / distance
          
          nextTile = getTile $ (intersectionCell i)
          
          heightFloorRanges :: Range -> ChunkSide -> Double -> FloorTex -> (Range, DL.DList (Range, RenderData))
          heightFloorRanges window side height tex = 
                  let (nextWindow, floorRange) = chunkWindow window side $ heightDistanceY viewParams distanceInv height
                  in  (nextWindow, DL.map (flip (,) (RenderData column ray i $ FloorType height tex)) $ splitFloorRange side height floorRange floorChunkWorldSpaceStart)
          
          createFloorRanges :: ChunkSide -> Tile -> Range -> (Range, DL.DList (Range, RenderData))
          createFloorRanges Bottom (Open (Floor (height, _) tex _) _) window = heightFloorRanges window Bottom height tex
          createFloorRanges Top (Open _ (Floor (height, _) tex _)) window = heightFloorRanges window Top height tex
          createFloorRanges _ _ window = (window, DL.empty)
          
          wallRanges :: Range -> ChunkSide -> Double -> Maybe Double -> WallTex -> Int -> (Range, DL.DList (Range, RenderData))
          wallRanges window side height bound tex defaultTex =
                  let
                    (nextWindow, wallRange) = chunkWindow window side $ heightDistanceY viewParams distanceInv height                    
                    boundY = maybeMap (heightDistanceY viewParams distanceInv) bound
                    (baseWallRange, boundRanges) = splitRange wallRange side boundY
                    walls = (baseWallRange, RenderData column ray i $ WallType distance tex) `DL.cons` (DL.map (flip (,) (RenderData column ray i $ WallType distance $ WallTex defaultTex WallSky)) boundRanges)
                  in (nextWindow, walls)
          
          createWallRanges :: ChunkSide -> Tile -> Int -> Range ->(Range, DL.DList (Range, RenderData))
          createWallRanges Bottom (Open (Floor (height, bound) _ tex) _) defaultTex window = wallRanges window Bottom height bound tex defaultTex
          createWallRanges Top (Open _ (Floor (height, bound) _ tex)) defaultTex window = wallRanges window Top height bound tex defaultTex
          createWallRanges _ (Wall tex) defaultTex window =
                  let wall = DL.singleton (window, RenderData column ray i $ WallType distance tex)
                  in (invalidRange, wall)
          createWallRanges _ _ defaultTex window = (window, DL.empty)

          (nextWindow, allRanges) = 
                                     (createFloorRanges Bottom currentTile)                         
                      `thenSections` (createFloorRanges Top currentTile)
                      `thenSections` (createWallRanges Bottom nextTile $ vpBottomSkyTex viewParams)
                      `thenSections` (createWallRanges Top nextTile $ vpTopSkyTex viewParams)       
                                            $ window

          moreOk = case nextTile of (Wall _) -> False
                                    _        -> rangeValid nextWindow
          
          more = if moreOk then findRanges nextTile nextWindow is else DL.empty
          
        in DL.append allRanges more
    columnRanges = findRanges startTile (Range (0, rpHeight params - 1)) $ collapseIntersections startTile $ all
  in map sectionFromRange . filter (rangeValid . fst) $ DL.toList $ columnRanges