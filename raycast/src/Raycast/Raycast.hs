module Raycast.Raycast(renderColumn, ViewParams(..), FloorTex(..), WallTex(..), FloorUV(..), WallUV(..), HorzSurface(..), WallSurface(..), Volume(..), GeometryIntersection(..), WallIntersection(..), Geometry(..), renderGeometry) where

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

data HorzSurface = NoSurface | HorzSurface (Double, Maybe Double) FloorTex deriving (Show, Eq)
data WallSurface = NoWall | WallSurface WallTex

data Volume = Volume { vBottom :: HorzSurface, vTop :: HorzSurface } deriving (Show, Eq)
data WallIntersection = Wall WallTex | Open WallSurface WallSurface

data GeometryIntersection = GI { giDistance :: Double, giUCoord :: Double, giWall :: WallIntersection, giNextVolume :: Volume }

class Geometry a where
  getVolume :: Vec2 -> a -> Volume
  intersect :: Ray -> a -> [GeometryIntersection]

data RenderSurfaceData = WallType Double WallTex | FloorType Double FloorTex
data RenderData = RenderData Int Ray GeometryIntersection RenderSurfaceData

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

data ViewParams = ViewParams { vpBottomSkyTex :: Int, vpTopSkyTex :: Int, vpHorizonY :: Double, vpHeightScale :: Double, vpTileRenderDist :: Int, vpEyeHeight :: Double, vpPlaneDist :: Double }

floorChunkWorldSpaceStart :: Double
floorChunkWorldSpaceStart = 0.15

floorChunkWorldSpaceIncrease :: Double
floorChunkWorldSpaceIncrease = 0.03

heightDistanceY :: ViewParams -> Double -> Double -> Int
heightDistanceY (ViewParams _ _ horizonY heightScale _ eyeHeight _) distanceInv height = round ((height - eyeHeight) * distanceInv * heightScale + horizonY)

yDistanceHeight :: ViewParams -> Int -> Double -> Double
yDistanceHeight (ViewParams _ _ horizonY heightScale _ eyeHeight _) y distance = eyeHeight + distance * ((fromIntegral y - horizonY) / heightScale)

yHeightDistance :: ViewParams -> Int -> Double -> Double
yHeightDistance (ViewParams _ _ horizonY heightScale _ eyeHeight _) y height = (height - eyeHeight) / ((fromIntegral y - horizonY) / heightScale)

type ProduceFromRange = Range -> (Range, DL.DList (Range, RenderData))

thenSections :: ProduceFromRange -> ProduceFromRange -> ProduceFromRange
thenSections f1 f2 r =
  let
    (window, rs) = f1 r
    (window', rs') = f2 window
  in (window', rs `DL.append` rs')
infixl 7 `thenSections`

renderGeometry :: (Geometry a) => RenderParams -> a -> ViewParams -> Ray -> [ColumnSection]
renderGeometry renderParams geometry viewParams view = 
      let        
        w = rpWidth renderParams
        halfColumn = floor $ fromIntegral w / 2
        
        viewVec@(Vec2 viewVecX viewVecY) = normalize $ rayDelta view
        forward = viewVec &* vpPlaneDist viewParams
        right = Vec2 viewVecY (-viewVecX)
        
        start = rayStart view
        sections = foldr (++) [] $ (flip map) [0..(w-1)] $
            \i -> let
                    dir = normalize $ forward &+ (right &* realToFrac (i - halfColumn))
                  in renderColumn renderParams i (mkRay start dir) viewVec viewParams geometry
      in sections

renderColumn :: (Geometry a) => RenderParams -> Int -> Ray -> Vec2 -> ViewParams -> a -> [ColumnSection]
renderColumn renderParams column ray forward viewParams geometry = let
    startVolume = getVolume (rayStart ray) geometry
    all = take (vpTileRenderDist viewParams) $ intersect ray geometry
    distScale = rayDelta ray &. normalize forward
    
    -- take the list of intersections, and pick out only the ones where the floors change (if the floors don't change, there will be no wall segments)
    collapseIntersections :: Volume -> [GeometryIntersection] -> [GeometryIntersection]
    collapseIntersections v [] = []
    collapseIntersections v [i] = [i]
    collapseIntersections v (i@(GI _ _ (Wall _) nextVolume):is) = i : collapseIntersections nextVolume is
    collapseIntersections v (i@(GI _ _ _ nextVolume):is) | v == nextVolume = collapseIntersections nextVolume is
                                                         | otherwise = i : collapseIntersections nextVolume is
    
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
    
    wallUV :: WallUV -> Ray -> GeometryIntersection -> Double -> Vec2
    wallUV WallFixed ray i height = Vec2 (giUCoord i) height
    wallUV WallSky ray i height = rayDelta ray &* (giDistance i / distScale)
    
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
    findRanges :: Volume -> Range -> [GeometryIntersection] -> DL.DList (Range, RenderData) -> DL.DList (Range, RenderData)
    findRanges currentVolume window [] accum = accum
    findRanges currentVolume window (i:is) accum =
        let
          distance = giDistance i * distScale
          distanceInv = 1.0 / distance
          
          hitWall = giWall i
          nextVolume = giNextVolume i
          
          heightFloorRanges :: Range -> ChunkSide -> Double -> FloorTex -> (Range, DL.DList (Range, RenderData))
          heightFloorRanges window side height tex = 
                  let (nextWindow, floorRange) = chunkWindow window side $ heightDistanceY viewParams distanceInv height
                  in  (nextWindow, DL.map (flip (,) (RenderData column ray i $ FloorType height tex)) $ splitFloorRange side height floorRange floorChunkWorldSpaceStart)
          
          createFloorRanges :: ChunkSide -> HorzSurface -> Range -> (Range, DL.DList (Range, RenderData))
          createFloorRanges side (HorzSurface (height, _) tex) window = heightFloorRanges window side height tex
          createFloorRanges _ _ window = (window, DL.empty)
          
          wallRanges :: Range -> ChunkSide -> Double -> Maybe Double -> WallTex -> Int -> (Range, DL.DList (Range, RenderData))
          wallRanges window side height bound tex defaultTex =
                  let
                    (nextWindow, wallRange) = chunkWindow window side $ heightDistanceY viewParams distanceInv height                    
                    boundY = maybeMap (heightDistanceY viewParams distanceInv) bound
                    (baseWallRange, boundRanges) = splitRange wallRange side boundY
                    walls = (baseWallRange, RenderData column ray i $ WallType distance tex) `DL.cons` (DL.map (flip (,) (RenderData column ray i $ WallType distance $ WallTex defaultTex WallSky)) boundRanges)
                  in (nextWindow, walls)
                  
          createSideWallRanges :: ChunkSide -> HorzSurface -> WallSurface -> Int -> Range -> (Range, DL.DList (Range, RenderData))
          createSideWallRanges side (HorzSurface (height, bound) _) (WallSurface tex) defaultTex window = wallRanges window side height bound tex defaultTex
          createSideWallRanges side _ _ defaultTex window = (window, DL.empty)
          
          createWallRanges :: WallIntersection -> Volume -> Int -> Int -> Range -> (Range, DL.DList (Range, RenderData))
          createWallRanges (Open wallBottom wallTop) (Volume vBottom vTop) defTexBottom defTexTop window =
                           createSideWallRanges Bottom vBottom wallBottom defTexBottom
            `thenSections` createSideWallRanges Top vTop wallTop defTexTop
                                    $ window
          createWallRanges (Wall tex) _ _ _ window =
                  let wall = DL.singleton (window, RenderData column ray i $ WallType distance tex)
                  in (invalidRange, wall)

          (nextWindow, allRanges) = 
                                     (createFloorRanges Bottom $ vBottom currentVolume)                         
                      `thenSections` (createFloorRanges Top $ vTop currentVolume)
                      `thenSections` (createWallRanges hitWall nextVolume (vpBottomSkyTex viewParams) (vpTopSkyTex viewParams))
                                            $ window

          moreOk = case hitWall of (Wall _) -> False
                                   _        -> rangeValid nextWindow
          
          moreIntersections = if moreOk then is else []
          -- more = if moreOk then findRanges nextVolume nextWindow is else DL.empty
          
        in findRanges nextVolume nextWindow moreIntersections (DL.append accum allRanges)
    columnRanges = findRanges startVolume (Range (0, rpHeight renderParams - 1)) (collapseIntersections startVolume $ all) DL.empty
  in map sectionFromRange . filter (rangeValid . fst) $ DL.toList $ columnRanges