module LiquidEngine where

import Data.Map (Map)
import qualified Data.Map as Map

import Grid
import Types

 
dt = 0.01::PositionT -- time step of simulation
m = 1.0::PositionT -- mass
d = 1.0::PositionT -- clipping distance for calculations
k = 10.0::PositionT -- 70 -- stiffness
mu = 0.3::PositionT -- viscosity coeff
sigma = 3.0::PositionT -- 50.00-- surface tension coeff
douter = 0.5::PositionT -- outer pressure
bound = 5.0::PositionT -- half of cube size
energyLoss = 0.1::PositionT
g = (9.81::PositionT) -- gravity constant
gravityVec = (0.0, -1.0, 0.0)::VecT -- where gravity pushes

cellOf :: Particle -> Cell
cellOf p = (n, m, k)
  where
    (x, y, z) = pos p
    n = floor (x / d)
    m = floor (y / d)
    k = floor (z / d)
 
nearCells :: Cell -> [Cell]
nearCells (i, j, k) = [ (i + di, j + dj, k + dk)
                      | di <- [-1..1]
                      , dj <- [-1..1]
                      , dk <- [-1..1] ]
 
step :: [Particle] -> [Particle]
step xs = ys
  where
    g  = fromList xs cellOf
    g' = update nearCells calcDensities g
    ys = values $ update nearCells processParticle g'
 
getDensity :: Particle -> Particle -> PositionT
getDensity a b = m * (wpoly6 d $ vLength $ (pos a) .- (pos b))

calcDensities :: [Particle] -> Particle -> Particle
calcDensities ps p = p { dencity = sum $ map (getDensity p) ps }


getForce :: Particle -> Particle -> VecT
getForce a b = ((pressure - tension) .* (pa .- pb)) .- (viscosity .* (va .- vb)) 
               where 
                 da  = dencity a
                 db  = dencity b
                 pa  = pos a
                 pb  = pos b
                 va  = velocity a
                 vb  = velocity b
                 len = vLength (pb .- pa)
                 w'  = wpoly6d d len
                 w'' = wpoly6dd d len
                 wv  = wvisc d len
                 pressure  = m * (k * (da + db - 2 * douter * w')) / (2 * db)
                 tension   = m * sigma * w'' / db
                 viscosity = m * mu * wv /db
                 

hitBounds :: Particle -> Particle
hitBounds p | x > bound  = hitBounds p { pos = (bound, y, z),  velocity = et .* (-v, w, h) }
            | z > bound  = hitBounds p { pos = (x, y, bound),  velocity = et .* (v, w, -h) }
            | x < -bound = hitBounds p { pos = (-bound, y, z), velocity = et .* (-v, w, h) }
            | z < -bound = hitBounds p { pos = (x, y, -bound), velocity = et .* (v, w, -h) }
            | y < 0      = hitBounds p { pos = (x, 0, z),      velocity = et .* (v, -w, h) }
            | otherwise = p
            where (x, y, z) = pos p
                  (v, w, h) = velocity p
                  et = (1 - energyLoss)

processParticle :: [Particle] -> Particle -> Particle
processParticle ps p = hitBounds p { pos = (pos p) .+ (dt .* vel), velocity = vel }
                       where 
                       	 force   = gravity .+ (foldl1 (.+) $ map (getForce p) ps)
                         vel     = (velocity p) .+ (dt .* force)
                         gravity = g .* gravityVec
 
(.+) :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
(x, y, z) .+ (x', y', z') = (x + x', y + y', z + z')

(.-) :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
(x, y, z) .- (x', y', z') = (x - x', y - y', z - z')

(.*) :: Num a => a -> (a, a, a) -> (a, a, a)
c .* (x, y, z) = (c * x, c * y, c * z)

neg :: Num a => (a, a, a) -> (a, a, a)
neg (x, y, z) = (-x, -y, -z)



vLength :: VecT -> PositionT
vLength (x, y, z) = sqrt (x^2 + y^2 + z^2) 

wconst:: PositionT -> PositionT
wconst h = 315 / (64*pi*h^9)

wpoly6 :: PositionT -> PositionT -> PositionT
wpoly6 h r | r<h =  (1 - r/h)^2 --  wconst h * (h^2-r^2)^3 -- 
           | otherwise = 0

wpoly6d:: PositionT -> PositionT -> PositionT
wpoly6d h r | r<h =  (wconst h) * 6 * r * (h^2-r^2)^2
            | otherwise = 0

wpoly6dd:: PositionT -> PositionT -> PositionT
wpoly6dd h r | r<h = (wconst h) * 6 * (h^2-r^2) * (4*r^2-(h^2-r^2))
             | otherwise = 0

wvisc :: PositionT -> PositionT -> PositionT
wvisc h r | r<h = 45 * (h - r) / (pi*h^6) 
          | otherwise = 0














