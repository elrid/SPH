import Graphics.UI.GLUT
--import Graphics.Rendering.OpenGL.Raw.Core31
import Data.IORef
import Bindings
import Points
import Types
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, WithAlphaComponent, DoubleBuffered, Multisampling, WithSamplesPerPixel 4]
  --marshalContextProfile <-glut_CORE_PROFILE
  --setContextVersion (3,2)
  _window <- createWindow "Fluid simulation"
  windowSize $= (Size 500 (500::GLsizei))
  reshapeCallback $= Just reshape
  depthFunc $= Just Less -- the comparison function for depth the buffer
  s <- newIORef (State (0.1, 0) (points 6)) -- it's 2 * n^3 amount of points
  lighting $= Enabled
  
  ambient (Light 0) $= Color4 0.1 0.1 0.5 (0.1::GLfloat)
  diffuse (Light 0) $= Color4 0.3 0.3 0.9 (0.1::GLfloat)
  specular (Light 0) $= Color4 1.0 1.0 1.0 (1.0::GLfloat)
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 2 0 (1::GLfloat)

  ambient (Light 1) $= Color4 0.1 0.1 0.5 (0.1::GLfloat)
  diffuse (Light 1) $= Color4 0.3 0.3 0.9 (0.1::GLfloat)
  specular (Light 1) $= Color4 1.0 1.0 1.0 (1.0::GLfloat)
  light (Light 1) $= Enabled
  position (Light 1) $= Vertex4 1 1 1 (1::GLfloat)

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  keyboardMouseCallback $= Just (keyboardMouse s)
  idleCallback $= Just (idle s)
  displayCallback $= display s
  mainLoop