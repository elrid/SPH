module Display (idle, display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points
import LiquidEngine
import Types

display :: IORef State  -> DisplayCallback
display s = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  loadIdentity
  translate (Vector3 0 (-0.6::GLfloat) 0)
  state <- get s
  let (x,y)  = camera state
      liquid = particles state
    in
    preservingMatrix $ do
      rotate (-10.0::PositionT) (Vector3 1.0 0 (0.0::PositionT))
      rotate (y) $ Vector3 0 1.0 0 
      scale x x x
      translate (Vector3 0 bound 0)
      cubeFrame bound
      translate (Vector3 0 (-bound) 0)
      forM_ (map pos liquid) $ \(x,y,z) -> preservingMatrix $ do
        color $ Color4 0.5 0.5 1.0 (0.3::PositionT) -- ((x+1)/2) ((y+1)/2) ((z+1)/2) (0.5)
        translate $ Vector3 x y z
        --scale 0.1 0.1 (0.1::PositionT)
        renderObject Solid (Sphere' 0.1 10 10)
        color $ Color4 (1::PositionT) 1 1 1 -- set outline color to black
  swapBuffers

 
idle :: IORef State -> IdleCallback
idle s = do
  s $~! \st -> st { particles = step (particles st) }
  postRedisplay Nothing