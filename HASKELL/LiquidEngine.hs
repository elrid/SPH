module LiquidEngine where

import Graphics.UI.GLUT
import Data.Map (Map)
import qualified Data.Map as Map

Data Vec3 = Vec3 {
    x::GLfloat
    y::GLfloat
    z::GLfloat
}

Data Point = Point {
    pos::Vec3
    vel::Vec3
    force::Vec3
}


m = 1-- mass
d = 1.0 -- clipping distance for calculations
k = 10 -- 70 -- stiffness
mu = 3 -- viscosity coeff
sigma = 5 -- 50.00-- surface tension coeff
douter = 5
bound = 5
energyLoss = 0.1



mapp :: [a->b] -> [a] -> [b]
mapp (x:xs) (y:ys) = (x y):(mapp xs ys)
mapp [] _ = []
mapp _ _ = [] 

map2arg :: (a->b->c) -> [a] -> [b] -> [c]
map2arg f (x:xs) (y:ys) = (f x y):(map2arg f xs ys)
map2arg _ _ _ = []

mktuple :: [a] -> [b] -> [(a,b)]
mktuple (x:xs) (y:ys) = (x,y):(mktuple xs ys)
mktuple [] _ = []
mktuple _ _ = []

.+ :: Vec3 -> Vec3 -> Vec3
.+ a b = Vec3 (a.x+b.x) (a.y+b.y) (a.z+b.z)

.- :: Vec3 -> Vec3 -> Vec3
.- a b = Vec3 (a.x-b.x) (a.y-b.y) (a.z-b.z)

.* :: GLfloat -> Vec3 -> Vec3
.* a p = Vec3 a*p.x a*p.y a*p.z

negp :: Vec3 -> Vec3
negp vect = Vec3 (negate vect.x) (negate vect.y) (negate vect.z) 


-- position = position + dt*velocity
-- velocity = velocity + dt*acceleration
move :: [Point] -> GLfloat -> [Point]
move p:points dt = Point (p.pos .+ (dt .* p.vel ) ) (p.vel .+ (dt .* p.acc) ) (Vec3 0 0 0)  --mktuple ( mapp (map ( .+ ) (map ( dt .* ) (map snd points))) (map fst points) ) (map snd points)

checkbound :: Point -> Point
checkbound p | p.pos.x > bound   = (checkbound ( Point (Vec3 bound p.pos.y p.pos.z)    (Vec3 (- ek * p.vel.x) p.vel.y p.vel.z) p.acc))
             | p.pos.z > bound   = (checkbound ( Point (Vec3 p.pos.x p.pos.y bound)    (Vec3 p.vel.x p.vel.y (- ek * p.vel.z)) p.acc))
             | p.pos.x < - bound = (checkbound ( Point (Vec3 (-bound) p.pos.y p.pos.z) (Vec3 (- ek * p.vel.x) p.vel.y p.vel.z) p.acc))
             | p.pos.z < - bound = (checkbound ( Point (Vec3 p.pos.x p.pos.y (-bound)) (Vec3 p.vel.x p.vel.y (- ek * p.vel.z)) p.acc))
             | p.pos.x < 0       = (checkbound ( Point (Vec3 p.pos.x p.pos.y 0)        (Vec3 p.vel.x - ek * p.vel.y p.vel.z)   p.acc))
             | otherwise = p
                                where
                                    ek = 1 - energyLoss

checkbounds :: [Point] -> [Point]
checkbounds pts = map checkbound pts


wconst:: GLfloat -> GLfloat
wconst h = 315 / (64*pi*h^9)

wpoly6 :: GLfloat -> GLfloat -> GLfloat
wpoly6 h r | r<h =  (1 - r/h)^2 --  wconst h * (h^2-r^2)^3 -- 
           | otherwise = 0

wpoly6d:: GLfloat -> GLfloat -> GLfloat
wpoly6d h r | r<h =  (wconst h) * 6 * r * (h^2-r^2)^2
            | otherwise = 0

wpoly6dd:: GLfloat -> GLfloat -> GLfloat
wpoly6dd h r | r<h = (wconst h) * 6 * (h^2-r^2) * (4*r^2-(h^2-r^2))
             | otherwise = 0

wvisc :: GLfloat -> GLfloat -> GLfloat
wvisc h r | r<h = 45 * (h - r) / (pi*h^6) 
          | otherwise = 0

dist :: Vec3 -> Vec3 -> GLfloat
dist a b = sqrt ( (a.x-b.x)^2 + (a.y-b.y)^2 + (a.z-b.z)^2 )

-- if position one in "nearby" cube of position 2, and vice versa
isnearby :: Vec3 -> Vec3 -> Bool
isnearby a b | ((abs (a.x - b.x)) < d) && ((abs (a.y - b.y)) < d) && ((abs (a.z - b.z)) < d) = True
             | otherwise = False

dencity :: [Point] -> Point -> GLfloat
dencity ((pt:pts) x | isnearby x.pos pt.pos = (m * (wpoly6 d (dist pt.pos x.pos))) + ( dencity pts x )
                    | otherwise             = ( dencity pts x )
dencity [] _ = m

pressure :: [(Vec3,Vec3)] -> [GLfloat] -> (Vec3,Vec3) -> GLfloat -> Vec3 
pressure (y:ys) (dr:ds) (x,vx) dc | isnearby x y = ( ( m * ( k * (dc + dr - 2 * douter) / (2 * dr) ) * (wpoly6d d (dist x y) )) .* (x .- y)) .+ (pressure ys ds (x,vx) dc )
                                 | otherwise          = (pressure ys ds (x,vx) dc)
pressure _ _ _ _ = (0,0,0)

viscosity :: [(Vec3,Vec3)] -> [GLfloat] -> (Vec3,Vec3) -> Vec3
viscosity ((y,vy):ys) (dr:ds) (x,vx) | isnearby x y =  ( (mu * m/dr * (wvisc d (dist x y))) .* (vy .- vx))  .+ (viscosity ys ds (x,vx)) 
                               | otherwise          = (viscosity ys ds (x,vx))
viscosity [] _ _ = (0,0,0)

tension :: [(Vec3,Vec3)] -> [GLfloat]  -> (Vec3,Vec3) -> Vec3
tension ((y,vy):ys) (dr:ds) (x,vx) | isnearby x y =  ( (sigma * m / dr * (wpoly6dd d (dist x y))) .* (y .- x) ) .+ (tension ys ds (x,vx))
                             | otherwise          = (tension ys ds (x,vx))
tension [] _ _ = (0,-9.8*m,0)

.++ :: [Vec3]->[Vec3]->[Vec3]
.++ (x:xs) (y:ys) = (.+ x y):(.++ xs ys)
.++ _ _ = []

addForces :: GLfloat -> [(Vec3,Vec3)] -> [Vec3] -> [(Vec3,Vec3)]
addForces dt ((p,v):pts) (f:fcs) = (p, (.+ v (.* (dt/m) f))):(addForces dt pts fcs)
addForces _ _ _ = []

calcForces :: Map String [(Vec3, Vec3)] -> [Vec3] 
calcForces pts = (let dens = (map (dencity pts) pts) in ( (map (tension pts dens) pts) .++ (map (viscosity pts dens) pts) .++ (map2arg (pressure pts dens) pts dens) )) 

getCellHash :: Point -> String
getCellHash (x,v) = ( show round (x/d) ) ++ "." ++ ( show round (y/d) ) ++ "." ++ ( show round (z/d) )

getNeighbours :: Map String [(Vec3,Vec3)] -> (Vec3,Vec3) -> [(Vec3,Vec3)]
getNeighbours points (x,v) = map ++ ( map (points ! ) ( map getCellHash map (x .+ ) [(xd,yd,zd) | xd <- [-1,0,1], yd <- [-1,0,1], zd <-[-1,0,1]] ) )

process :: GLfloat -> Map String [Point] -> Map String [Point]
process dt points = checkbounds (move (addForces dt points (calcForces points)) dt)















