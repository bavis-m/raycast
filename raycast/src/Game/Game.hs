{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Game.Game (GameState, updateState, Viewpoint(..), parseAndLoad) where

import Data.Maybe
import Debug.Trace

import Data.Vect.Double.Base

import Engine.RuntimeInterface
import Raycast.Grid
import Game.Level
import Game.Input
import Game.Keys
import Raycast.Intersection
import Raycast.Raycast
import Util.Util

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (unpack, pack, Text)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import Data.List
import qualified Data.Map.Strict as Map

data Viewpoint = Viewpoint { vAngle :: Double, vPos :: Vec2, vHorizon :: Double, vEyeHeight :: Double, vPlaneDist :: Double }

data GameState = GameState InputState (Level Tile) (Int, Int) Viewpoint

data HalfTile = None | Floor (Double, Maybe Double) FloorTex WallTex deriving (Show)
data Tile = TileWall WallTex | TileOpen HalfTile HalfTile deriving (Show)

halfTileSurface :: HalfTile -> HorzSurface
halfTileSurface None = NoSurface
halfTileSurface (Floor h tex _) = HorzSurface h tex

tileVolume :: Tile -> Volume
tileVolume (TileWall _) = Volume NoSurface NoSurface
tileVolume (TileOpen bottom top) = Volume (halfTileSurface bottom) (halfTileSurface top)

instance Geometry (Level Tile) where
  getVolume v level = tileVolume $ tile level $ mkCell v
  {-# INLINE getVolume #-}
  
  intersect ray level = let
      wallSurfaceFromHalfTile (Floor _ _ tex) = WallSurface tex
      wallSurfaceFromHalfTile _ = NoWall
      
      wallFromTile (TileWall tex) = Wall tex
      wallFromTile (TileOpen bottom top) = Open (wallSurfaceFromHalfTile bottom) (wallSurfaceFromHalfTile top)
      
      horzSurfaceFromHalfTile None = NoSurface
      horzSurfaceFromHalfTile (Floor r tex _) = HorzSurface r tex
      
      volumeFromTile (TileWall _) = Volume NoSurface NoSurface
      volumeFromTile (TileOpen bottom top) = Volume (horzSurfaceFromHalfTile bottom) (horzSurfaceFromHalfTile top)
      
      geometryIntersection i = let
          intersectionTile = tile level $ intersectionCell i
        in GI (tValue i) (collisionU ray i) (wallFromTile intersectionTile) (volumeFromTile intersectionTile)
    in fmap geometryIntersection $ intersections ray
  {-# INLINE intersect #-}

forwards :: Double -> Vec2
forwards angle = mkVec2 (- (sin angle), cos angle)

right :: Double -> Vec2
right angle = mkVec2 (cos angle, sin angle)

instance Renderable GameState where
  render rp (GameState input level (bottomSky, topSky) view) = let
      viewParams = (ViewParams bottomSky topSky (vHorizon view) 380 30 (vEyeHeight view) (vPlaneDist view))
      ray = mkRay (vPos view) $ forwards $ vAngle view
      sections = renderGeometry rp level viewParams ray
    in RenderFrame sections

ifVal :: Bool -> Double -> Double
ifVal b v | b = v
          | otherwise = 0
      
updateState :: UpdateFrame -> GameState -> GameState
updateState uf (GameState input l skyTex view) = let
    deltaTime = ufTime uf
    newInput = updateInputState uf input
    
    newAngle = vAngle view - (realToFrac $ ufMouseRelX uf) * 0.004
    newHorizon = max (-50) $ min 600 $ vHorizon view + (realToFrac $ ufMouseRelY uf)
    
    moveX = right newAngle &* (ifVal (keyDown newInput keyD) 1 + ifVal (keyDown newInput keyA) (-1))
    moveY = forwards newAngle &* (ifVal (keyDown newInput keyW) 1 + ifVal (keyDown newInput keyS) (-1))
    move = normalizeSafe ((moveX &* deltaTime) &+ (moveY &* deltaTime)) &* (deltaTime * 4)
    newEyeHeight = vEyeHeight view + (ifVal (keyDown newInput keyQ) 1 + ifVal (keyDown newInput keyE) (-1)) * deltaTime
    
    
  in GameState newInput l skyTex $ view { vAngle = newAngle, vPos = vPos view &+ move, vEyeHeight = newEyeHeight, vHorizon = newHorizon }

  
-- loading
data LevelDef = LevelDef
                {
                  ldBottomSky :: Maybe String,
                  ldTopSky :: Maybe String,
                  ldTileDefs :: (Map.Map Char TileDef),
                  ldWidth :: Int,
                  ldHeight :: Int,
                  ldDataBottom :: [Char],
                  ldDataTop :: [Char]
                } deriving (Show)

data TileDef = TDEmpty |
               TDWall (Maybe (String, WallUV)) |
               TDFloor Double (Maybe Double) (Maybe (String, FloorUV)) (Maybe (String, WallUV)) deriving (Show)

tileTextures :: TileDef -> [String]
tileTextures (TDWall (Just (tex, _))) = [tex]
tileTextures (TDFloor _ _ floor walls) = catMaybes $ [fmap fst $ floor, fmap fst $ walls]
tileTextures _ = []

skipLineSpace = skipWhile isHorizontalSpace
lineEnd = (endOfInput <|> endOfLine)
line p = skipLineSpace *> p <* skipLineSpace <* lineEnd

parseTileDef :: Parser (Char, TileDef)
parseTileDef = do
  string "tile"
  skipLineSpace
  name <- anyChar
  skipLineSpace
  def <- parseWallDef <|> parseFloorDef  
  return (name, def)
  
parseFloorUV = option FloorFixed $ (skipLineSpace *> string "[sky]" *> return FloorSky)
parseWallUV = return WallFixed

parseTexture :: Parser a -> Parser (Maybe (String, a))
parseTexture uvParser = (string "none" *> return Nothing) <|> parseTexture'
  where
    parseTexture' = do
      name <- fmap unpack $ takeWhile1 $ inClass "a-zA-Z0-9_.-/\\"
      uv <- uvParser
      return $ Just (name, uv)

parseWallBounds :: Parser (Maybe Double, Maybe Double)
parseWallBounds =
  string "[" *>
  (   
    (
      do
        low <- double
        string ","
        high <- double
        return (Just low, Just high)
    ) <|>
    (fmap ((flip (,) Nothing) . Just) double <* string ",") <|>
    (string "," *> fmap (((,) Nothing) . Just) double)
    
  )
  <* "]"
      
parseWallDef :: Parser TileDef
parseWallDef = do
  string "wall"  
  skipLineSpace
  tex <- parseTexture parseWallUV
  return (TDWall tex)
  
parseFloorDef :: Parser TileDef
parseFloorDef = do
  height <- double
  bound <- option Nothing $ skipLineSpace *> string "[" *> (fmap Just double) <* string "]"
  skipLineSpace
  floor <- parseTexture parseFloorUV
  skipLineSpace
  walls <- parseTexture parseWallUV
  return (TDFloor height bound floor walls)
  
parseLevelDef :: Parser LevelDef
parseLevelDef = do
  tiles <- many (line parseTileDef)
  
  endOfLine
  
  (width, height) <- line $ (,) <$> (decimal <* skipLineSpace) <*> decimal
  
  endOfLine
  
  bottomSky <- line $ fmap (fmap fst) $ parseTexture (return FloorSky)
  
  charsBottom <- fmap concat $ count height $ (count width anyChar) <* lineEnd
  
  endOfLine
  
  topSky <- line $ fmap (fmap fst) $ parseTexture (return FloorSky)
  
  charsTop <- fmap concat $ count height $ (count width anyChar) <* lineEnd
  
  let map = foldl' (\m t -> Map.insert (fst t) (snd t) m) Map.empty ((' ', TDEmpty) : tiles)
  return $ LevelDef bottomSky topSky map width height charsBottom charsTop
  
parseStart :: Parser (Vec2, Double)
parseStart = do
  string "start"
  skipLineSpace
  x <- double
  skipLineSpace
  char ','
  skipLineSpace
  y <- double
  skipLineSpace
  angle <- double
  return (Vec2 x y, angle)

parseAndMore :: (Monad m) => Parser a -> Text -> ExceptT String m (a, Text)
parseAndMore p t = let
     parseAndMore' (Done more res) = return (res, more)
     parseAndMore' (Partial cont) = parseAndMore' $ cont ""
     parseAndMore' (Fail i cs err) = throwE $ "Error " ++ err ++ " in " ++ show cs ++ "  left: " ++ show i
  in parseAndMore' (parse p t)
  
toHalfTile :: Map.Map String Int -> Maybe String -> TileDef -> HalfTile
toHalfTile m _ (TDWall _) = error "Can't convert a TDWall to only a HalfTile"
toHalfTile m _ TDEmpty = None
toHalfTile m skyTex (TDFloor height bound floor walls) = Floor (height, bound) (floorTileTex m skyTex floor) (wallTileTex m skyTex walls)

floorTileTex :: Map.Map String Int -> Maybe String -> Maybe (String, FloorUV) -> FloorTex
floorTileTex m _ (Just (name, uv)) = FloorTex (m Map.! name) uv
floorTileTex m skyTex Nothing = FloorTex skyVal FloorSky
  where skyVal = maybe (-1) (m Map.!) skyTex

wallTileTex :: Map.Map String Int -> Maybe String -> Maybe (String, WallUV) -> WallTex
wallTileTex m _ (Just (name, uv)) = WallTex (m Map.! name) uv
wallTileTex m skyTex Nothing = WallTex skyVal WallSky
  where skyVal = maybe (-1) (m Map.!) skyTex
  
combineTileDefs :: Map.Map String Int -> (Maybe String, Maybe String) -> (TileDef, TileDef) -> Tile
combineTileDefs m _ (TDWall tex, _) = TileWall $ wallTileTex m Nothing tex
combineTileDefs m _ (_, TDWall tex) = TileWall $ wallTileTex m Nothing tex
combineTileDefs m (bottomSkyTex, topSkyTex) (floor, ceiling) = TileOpen (toHalfTile m bottomSkyTex floor) (toHalfTile m topSkyTex ceiling)
  
parseAndLoad :: String -> ExceptT String IO GameState
parseAndLoad contentsStr =
  do
    let contents = pack contentsStr
    (levelDef, contents) <- parseAndMore (parseLevelDef <* endOfLine) contents
    ((pos, angle), contents) <- parseAndMore (line parseStart) contents
    
    let tileTextureList = concat . fmap tileTextures . Map.elems . ldTileDefs $ levelDef
    let textures = nub (maybeToList (ldBottomSky levelDef) ++ maybeToList (ldTopSky levelDef) ++ tileTextureList)

    idsByTextureName <- fmap (Map.fromList . zip textures) $ lift $ loadTextures textures
    
    let tilesBottom = (fmap (((Map.!) $ ldTileDefs levelDef)) $ ldDataBottom levelDef)
    let tilesTop = (fmap (((Map.!) $ ldTileDefs levelDef)) $ ldDataTop levelDef)
    
    let level = mkLevel
                  (ldWidth levelDef)
                  (ldHeight levelDef)
                  (TileOpen None None)
                  (fmap (combineTileDefs idsByTextureName (ldBottomSky levelDef, ldTopSky levelDef)) $ zip tilesBottom tilesTop)
    let viewpoint = Viewpoint angle  pos 200 0.6 400
    
    return $ GameState initialInput level (-1, -1) viewpoint