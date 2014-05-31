module Types where
import Graphics.UI.GLUT

type PositionT = GLfloat
type VecT = (PositionT, PositionT, PositionT)

data State = State 
  {
    camera :: (PositionT, PositionT),
    particles :: [Particle]
  } 
data Particle = Particle
  { 
    pos :: VecT,
    velocity :: VecT,
    dencity :: PositionT

  }


type Cell = (Int, Int, Int)