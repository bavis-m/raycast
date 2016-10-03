{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.RuntimeInterface
  (
   initRuntime,
   runEngine,
   UpdateFrame(..),
   loadTextures,
   RenderParams(..),
   ColumnSection(..),
   Renderable(..),
   RenderFrame(..)
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.IORef
import Debug.Trace
import Control.Monad (forM)

type UpdateStateFun = Ptr RenderParams -> Ptr UpdateFrame -> IO (Ptr EngineFrame)

foreign import ccall "init" runtime_Init :: IO Bool
foreign import ccall "loadTexture" runtime_LoadTexture :: CWString -> IO CInt
foreign import ccall "run" runtime_Run :: FunPtr UpdateStateFun -> IO ()
foreign import ccall "wrapper" wrap :: UpdateStateFun -> IO (FunPtr UpdateStateFun)

loadTextures :: [String] -> IO [Int]
loadTextures names = forM names $ \name -> do
  cName <- newCWString name
  id <- runtime_LoadTexture cName
  case id of -1 -> error $ "Couldn't load texture " ++ name
             _ -> return $ fromIntegral id

initRuntime = runtime_Init >>= \success ->
  if success then do
    putStrLn "Runtime init successful"
    return True
  else do
    putStrLn "Failed to init runtime"
    return False
                                                                                 
runEngine :: (Renderable a) => a -> (UpdateFrame -> a -> a) -> IO ()
runEngine initialState updateState = do
  gameStateRef <- newIORef initialState
  renderInterfaceStateRef <- initRenderInterface 1024
  
  updateFunction <- wrap $ \renderParamsPtr updateFramePtr -> do
      renderParams <- peek renderParamsPtr
      updateFrame <- peek updateFramePtr
      newGameState <- fmap (updateState updateFrame) $ readIORef gameStateRef
      writeIORef gameStateRef newGameState  
      renderFrom renderInterfaceStateRef renderParams newGameState
  
  runtime_Run updateFunction
    
  freeRenderInterface renderInterfaceStateRef    
  freeHaskellFunPtr updateFunction    
    
  putStrLn "done"
                                                                                 
data ColumnSection = ColumnSection
                     {
                       column :: {-# UNPACK #-} !Int,
                       startY :: {-# UNPACK #-} !Int,
                       endY :: {-# UNPACK #-} !Int,
                       tex :: {-# UNPACK #-} !Int,
                       startU :: {-# UNPACK #-} !Double,
                       startV :: {-# UNPACK #-} !Double,
                       endU :: {-# UNPACK #-} !Double,
                       endV :: {-# UNPACK #-} !Double
                     } deriving (Show)
                     
data RenderParams = RenderParams { rpWidth :: Int, rpHeight :: Int } deriving (Show)

data RenderFrame = RenderFrame [ColumnSection]

data UpdateFrame = UpdateFrame { ufTime :: Double, ufKeysPressed :: [Int], ufKeysReleased :: [Int], ufMouseRelX :: Int, ufMouseRelY :: Int} deriving (Show)

data EngineFrame = EngineFrame { columnMemory :: Ptr ColumnSection, numColumnSections :: Int }

data RenderInterfaceState = RenderInterfaceState { engineFramePtr :: Ptr EngineFrame, columnSections :: Ptr ColumnSection, columnSectionSize :: Int }

class Renderable a where
  render :: RenderParams -> a -> RenderFrame

initRenderInterface :: Int -> IO (IORef RenderInterfaceState)
initRenderInterface initialColumnSectionsSize = do
  initialColumnSections <- mallocArray initialColumnSectionsSize
  engineFramePtr <- (malloc :: IO (Ptr EngineFrame))
  newIORef $ RenderInterfaceState engineFramePtr initialColumnSections initialColumnSectionsSize
  
freeRenderInterface :: IORef RenderInterfaceState -> IO ()
freeRenderInterface renderInterfaceStateRef = 
  do
    renderInterfaceState <- readIORef renderInterfaceStateRef
    free $ columnSections renderInterfaceState
    free $ engineFramePtr renderInterfaceState

renderFrom :: (Renderable a) => IORef RenderInterfaceState -> RenderParams -> a -> IO (Ptr EngineFrame)
renderFrom rendererStateRef renderParams renderable = do
  let (RenderFrame columns) = render renderParams renderable
  renderInterfaceState <- readIORef rendererStateRef
  (newRendererState, engineFrame) <- writeColumnSections renderInterfaceState columns 0
  writeIORef rendererStateRef newRendererState
  poke (engineFramePtr newRendererState) engineFrame
  return $ engineFramePtr newRendererState
  where
    writeColumnSections :: RenderInterfaceState -> [ColumnSection] -> Int -> IO (RenderInterfaceState, EngineFrame)
    writeColumnSections rs [] written = return (rs, EngineFrame (columnSections rs) written)
    writeColumnSections rs (cs:css) written | written < columnSectionSize rs = do
      poke (plusTypedPtr (columnSections rs) written) cs
      writeColumnSections rs css $ written + 1
    writeColumnSections rs sections written | otherwise = do
      newRs <- ensureNumColumnSections (columnSectionSize rs * 2) rs
      writeColumnSections newRs sections written
      
ensureNumColumnSections :: Int -> RenderInterfaceState -> IO RenderInterfaceState
ensureNumColumnSections num rs | columnSectionSize rs >= num = return rs
                               | otherwise = do
  let newSize = traceShowId $ columnSectionSize rs * 2
  newPtr <- reallocArray (columnSections rs) newSize
  -- iterate to make sure we double until we have enough iterations
  ensureNumColumnSections num $ rs { columnSections = newPtr, columnSectionSize = newSize }


---------------------
-- Storable instances  
---------------------

plusTypedPtr :: forall a. (Storable a) => Ptr a -> Int -> Ptr a
plusTypedPtr ptr i = plusPtr ptr (i * sizeOf (undefined :: a))

#include "HaskellStructs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable ColumnSection where
  alignment _ = #{alignment columnSection}
  sizeOf _ = #{size columnSection}
  peek ptr = do -- should never have to read this anyways...
    column <- #{peek columnSection, column} ptr :: IO CInt
    startY <- #{peek columnSection, startY} ptr :: IO CInt
    endY <- #{peek columnSection, endY} ptr :: IO CInt
    tex <- #{peek columnSection, tex} ptr :: IO CInt
    startU <- #{peek columnSection, startU} ptr :: IO CFloat
    startV <- #{peek columnSection, startV} ptr :: IO CFloat
    endU <- #{peek columnSection, endU} ptr :: IO CFloat
    endV <- #{peek columnSection, endV} ptr :: IO CFloat
    return (ColumnSection (fromIntegral column) (fromIntegral startY) (fromIntegral endY) (fromIntegral tex) (realToFrac startU) (realToFrac endU) (realToFrac startV) (realToFrac endV))
  poke ptr (ColumnSection column startY endY tex startU startV endU endV) = do
    #{poke columnSection, column} ptr (fromIntegral column :: CInt)
    #{poke columnSection, startY} ptr (fromIntegral startY :: CInt)
    #{poke columnSection, endY} ptr (fromIntegral endY :: CInt)
    #{poke columnSection, tex} ptr (fromIntegral tex :: CInt)
    #{poke columnSection, startU} ptr (realToFrac startU :: CFloat)
    #{poke columnSection, startV} ptr (realToFrac startV :: CFloat)
    #{poke columnSection, endU} ptr (realToFrac endU :: CFloat)
    #{poke columnSection, endV} ptr (realToFrac endV :: CFloat)
    
instance Storable RenderParams where
  alignment _ = #{alignment renderParams}
  sizeOf _ = #{size renderParams}
  peek ptr = do
    width <- #{peek renderParams, width} ptr :: IO CInt
    height <- #{peek renderParams, height} ptr :: IO CInt
    return (RenderParams (fromIntegral width) (fromIntegral height))
  poke ptr (RenderParams width height) = return ()
  -- should never have to write this
    -- do
      -- #{poke renderParams, width} ptr width
      -- #{poke renderParams, height} ptr height
                                
instance Storable EngineFrame where
  alignment _ = #{alignment engineFrame}
  sizeOf _ = #{size engineFrame}
  peek ptr = return (EngineFrame nullPtr 0)
  -- should never have to read this
    -- do
      -- columnMemory <- #{peek engineFrame, columnMemory} ptr
      -- numColumnSections <- #{peek engineFrame, numColumnSections} ptr
      -- return (EngineFrame columnMemory numColumnSections)
  poke ptr (EngineFrame columnMemory numColumnSections) = do
    #{poke engineFrame, columnMemory} ptr columnMemory
    #{poke engineFrame, numColumnSections} ptr (fromIntegral numColumnSections :: CInt)

instance Storable UpdateFrame where
  alignment _ = #{alignment updateFrame}
  sizeOf _ = #{size updateFrame}
  peek ptr = do
    time <- #{peek updateFrame, time} ptr :: IO CDouble
    keysPressedPtr <- #{peek updateFrame, keysPressed} ptr :: IO (Ptr CInt)
    numKeysPressed <- #{peek updateFrame, numKeysPressed} ptr :: IO CInt
    keysReleasedPtr <- #{peek updateFrame, keysReleased} ptr :: IO (Ptr CInt)
    numKeysReleased <- #{peek updateFrame, numKeysReleased} ptr :: IO CInt
    
    -- kind of ugly way to get the arrays without going to IO again    
    keysPressed <- peekArray (fromIntegral numKeysPressed) keysPressedPtr
    keysReleased <- peekArray (fromIntegral numKeysReleased) keysReleasedPtr
    
    mouseRelX <- #{peek updateFrame, mouseRelX} ptr :: IO CInt
    mouseRelY <- #{peek updateFrame, mouseRelY} ptr :: IO CInt
    
    return (UpdateFrame (realToFrac time) (fmap fromIntegral keysPressed) (fmap fromIntegral keysReleased) (fromIntegral mouseRelX) (fromIntegral mouseRelY))
  -- don't actually need to go this way! (hopefully)
  poke ptr _ = return ()