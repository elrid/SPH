module Display (idle, display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points
import LiquidEngine
 
boxsize = 5

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef [(Vec3, Vec3)] -> DisplayCallback
display angle pos lqpoints = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  loadIdentity
  (x',y') <- get pos
  lqpts <- get lqpoints
  translate (Vector3 0 (-0.6::GLfloat) 0)
  preservingMatrix $ do
    a <- get angle
    rotate (-10) (Vector3 1.0 0 (0.0::GLfloat))
    rotate (a+y') $ Vector3 0 1.0 0 
    scale x' x' x'
    translate (Vector3 0 (boxsize::GLfloat) 0)
    cubeFrame boxsize
    translate (Vector3 0 (-boxsize::GLfloat) 0)
    forM_ (map fst lqpts) $ \(x,y,z) -> preservingMatrix $ do
      color $ Color4 0.5 0.5 1.0 (0.3::GLfloat) -- ((x+1)/2) ((y+1)/2) ((z+1)/2) (0.5)
      translate $ Vector3 x y z
      --scale 0.1 0.1 (0.1::GLfloat)
      renderObject Solid (Sphere' 0.1 10 10)
      color $ Color4 (1::GLfloat) 1 1 1 -- set outline color to black

      
  swapBuffers
 
idle :: IORef GLfloat -> IORef GLfloat -> IORef [(Vec3,Vec3)] -> IdleCallback
idle angle delta lqpoints = do
  d <- get delta
  angle $~! (+ d)
  lqpoints $~! (process (0.1*d))
  postRedisplay Nothing