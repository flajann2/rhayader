{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.NEAT.Visualization.Izhikevich where

import Raylib.Core
  ( clearBackground
  , isKeyPressed
  , isMouseButtonDown
  , getMouseDelta
  , getMouseWheelMove
  )
import Raylib.Core.Models (drawGrid, drawLine3D, drawSphere)
import Raylib.Core.Text (drawText, drawFPS)
import Raylib.Types
  ( pattern Vector2
  , Vector3
  , pattern Vector3
  , pattern Rectangle
  , Color (..)
  , Camera3D (..)
  , CameraProjection (CameraPerspective)
  , KeyboardKey (KeyR)
  , MouseButton (MouseButtonRight)
  )
import Raylib.Util (whileWindowOpen0, withWindow, drawing, mode3D)
import Raylib.Util.Colors (white, red, green, blue)
import Raylib.Util.GUI (guiSlider, guiButton, guiLabel)
import Control.Monad (when)
import Data.IORef
import Engine.NEAT.Izhikevich

data Vec3 = Vec3 !Float !Float !Float deriving (Show, Eq)

vec3 :: Float -> Float -> Float -> Vec3
vec3 = Vec3

toVector3 :: Vec3 -> Vector3
toVector3 (Vec3 x y z) = Vector3 x y z


data CameraState = CameraState
  { orbitYaw      :: !Float
  , orbitPitch    :: !Float
  , orbitDistance :: !Float
  }

data Config = Config { windowWidth   :: Int
                     , windowHeight  :: Int
                     , neuronParams  :: IzhikevichParams Float
                     , initialCamera :: CameraState
                     , tau           :: Double  -- delay embedding lag
                     , initialI      :: Float   -- injected/synaptic current
                     , windowName    :: String
                     }

initialIzhikevichParams :: IzhikevichParams Float
initialIzhikevichParams = (IzhikevichParams 0.02 0.2 (-65.0) 8.0)

defaultConfig :: Config
defaultConfig = (Config 2560 1600
                        initialIzhikevichParams
                        (CameraState 0.8 0.5 45.0)
                        0.0
                        10.0
                        "3D Phase Portrait - Izhikevich Neuron, Delay Embedding"
                )
defaultI = 10.0  -- injected/synaptic current

initialNeuronState :: IzhikevichParams Float -> NeuronState Float
initialNeuronState ns = NeuronState ns.zc (ns.zb * ns.zc)

-- | Delay-embedding window: how many 1ms steps back to sample v(t-tau) from.
delaySteps :: Int
delaySteps = 20

maxHistory :: Int
maxHistory = delaySteps + 1

mouseSensitivity :: Float
mouseSensitivity = 0.005

zoomSensitivity :: Float
zoomSensitivity = 2.0

minDistance, maxDistance, maxPitch :: Float
minDistance = 5.0
maxDistance = 200.0
maxPitch    = 1.5

orbitToVector3 :: CameraState -> Vector3
orbitToVector3 (CameraState yaw pitch dist) =
  let cosPitch = cos pitch
      x = dist * cosPitch * sin yaw
      y = dist * sin pitch
      z = dist * cosPitch * cos yaw
  in Vector3 x y z

dt :: Float
dt = 0.5

run :: Config -> IO ()
run cfg = withWindow cfg.windowWidth cfg.windowHeight cfg.windowName 60 $ \_ -> do
  orbitRef      <- newIORef (CameraState 0.8 0.5 45.0)
  neuronRef     <- newIORef (initialNeuronState initialIzhikevichParams)
  vHistoryRef   <- newIORef ([] :: [Float])
  trailRef      <- newIORef ([] :: [Vec3])
  aRef          <- newIORef cfg.neuronParams.za
  bRef          <- newIORef cfg.neuronParams.zb
  cRef          <- newIORef cfg.neuronParams.zc
  dRef          <- newIORef cfg.neuronParams.zd
  iRef          <- newIORef cfg.initialI
  let maxPoints = 15000 :: Int

  whileWindowOpen0 $ do
    a    <- readIORef aRef
    b    <- readIORef bRef
    c    <- readIORef cRef
    d    <- readIORef dRef
    iCur <- readIORef iRef

    neuron <- readIORef neuronRef
    let neuron' = stepIzh dt (IzhikevichParams a b c d) iCur neuron
    writeIORef neuronRef neuron'

    vHist <- readIORef vHistoryRef
    let vHist' = take maxHistory (nV neuron' : vHist)
    writeIORef vHistoryRef vHist'

    let vDelayed =
          if length vHist' > delaySteps
            then vHist' !! delaySteps
            else nV neuron'
        embedPoint = vec3 (nV neuron') (nU neuron') vDelayed

    trail <- readIORef trailRef
    writeIORef trailRef (take maxPoints (embedPoint : trail))

    resetPressed <- isKeyPressed KeyR
    when resetPressed $ do
      writeIORef neuronRef neuron
      writeIORef trailRef []
      writeIORef vHistoryRef []
    dragging        <- isMouseButtonDown MouseButtonRight
    Vector2 mdx mdy <- getMouseDelta
    wheel           <- getMouseWheelMove

    modifyIORef' orbitRef $ \(CameraState yaw pitch dist) ->
      let (yaw', pitch') =
            if dragging
              then ( yaw   - mdx * mouseSensitivity
                   , clampPitch (pitch - mdy * mouseSensitivity)
                   )
              else (yaw, pitch)
          dist' = clampDistance (dist - wheel * zoomSensitivity)
      in CameraState yaw' pitch' dist'

    orbit <- readIORef orbitRef
    let camera =
          Camera3D
            (orbitToVector3 orbit)
            (Vector3 0 0 0)
            (Vector3 0 1 0)
            50.0
            CameraPerspective

    trailPoints <- readIORef trailRef

    drawing $ do
      clearBackground (Color 15 45 25 255)
      mode3D camera $ do
        drawGrid 50 1.0
        drawAxes 18
        drawTrail trailPoints
        drawSphere (toVector3 embedPoint) 0.6 (Color 255 70 70 255)
      drawText "Izhikevich Neuron -- Phase Portrait (v, u, v(t-tau))" 20 20 24 white
      drawText "Drag mouse to orbit * Scroll to zoom * R = reset" 20 50 18 (Color 200 200 220 255)
      drawFPS 20 80

      -- GUI overlay: a/b/c/d/I sliders + redraw button
      guiLabel (Rectangle 20 110 200 20) "Izhikevich Parameters"

      (_, a') <- guiSlider (Rectangle 100 135 200 20) (Just "a") (Just (showF a)) a 0.0 0.2
      writeIORef aRef a'

      (_, b') <- guiSlider (Rectangle 100 165 200 20) (Just "b") (Just (showF b)) b 0.0 0.3
      writeIORef bRef b'

      (_, c') <- guiSlider (Rectangle 100 195 200 20) (Just "c") (Just (showF c)) c (-80.0) (-30.0)
      writeIORef cRef c'

      (_, d') <- guiSlider (Rectangle 100 225 200 20) (Just "d") (Just (showF d)) d 0.0 10.0
      writeIORef dRef d'

      (_, i') <- guiSlider (Rectangle 100 255 200 20) (Just "I") (Just (showF iCur)) iCur (-10.0) 40.0
      writeIORef iRef i'

      redrawClicked <- guiButton (Rectangle 20 290 120 30) (Just "Redraw")
      when redrawClicked $ do
        writeIORef neuronRef (initialNeuronState initialIzhikevichParams)
        writeIORef trailRef []
        writeIORef vHistoryRef []
  where
    clampPitch :: Float -> Float
    clampPitch p = max (-maxPitch) (min maxPitch p)

    clampDistance :: Float -> Float
    clampDistance d = max minDistance (min maxDistance d)

    showF :: Float -> String
    showF x = show (fromIntegral (round (x * 100)) / 100 :: Float)

    drawAxes :: Float -> IO ()
    drawAxes s = do
      let o = Vector3 0 0 0
      drawLine3D o (Vector3 s 0 0) red
      drawLine3D o (Vector3 0 s 0) green
      drawLine3D o (Vector3 0 0 s) blue

    drawTrail :: [Vec3] -> IO ()
    drawTrail [] = return ()
    drawTrail [_] = return ()
    drawTrail (p1@(Vec3 _ _ z1) : p2@(Vec3 _ _ z2) : rest) = do
      let v1 = toVector3 p1
          v2 = toVector3 p2
          avgZ = (z1 + z2) / 2
          t    = max 0 (min 1 ((avgZ + 65) / 100))
          r = round (30 + 200 * t) :: Int
          g = round (120 + 100 * t) :: Int
          b = round (220 - 100 * t) :: Int
          col = Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
      drawLine3D v1 v2 col
      drawTrail (p2 : rest)
