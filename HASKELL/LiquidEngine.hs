module LiquidEngine where

import Graphics.UI.GLUT

type Vec3 = (GLfloat, GLfloat, GLfloat)

k = 50
m = 1-- mass
d = 1.0 -- clipping distance for calculations
mu = 3.00 -- viscosity coeff
sigma = 50.00-- surface tension coeff
douter = 0.50

frst (a,b,c) = a
scnd (a,b,c) = b
thrd (a,b,c) = c

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

addp :: Vec3 -> Vec3 -> Vec3
addp (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

subp :: Vec3 -> Vec3 -> Vec3
subp (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

mulp :: GLfloat -> Vec3 -> Vec3
mulp a (x, y, z) = (a*x, a*y, a*z)

negp :: Vec3 -> Vec3
negp (x, y, z) = (negate x, negate y, negate z)

move :: [(Vec3,Vec3)] -> GLfloat -> [(Vec3,Vec3)]
move points dt = mktuple ( mapp (map addp (map (mulp dt) (map snd points))) (map fst points) ) (map snd points)

checkbound :: (Vec3,Vec3) -> (Vec3,Vec3)
checkbound ((x,y,z),(fx,fy,fz)) | x>2.0 = (checkbound ((1.999 - fx*0.01,y,z),(-0.3*fx,fy,fz)))
                                | z>2.0 = (checkbound ((x,y,1.999 - fz*0.01),(fx,fy,-0.3*fz)))
                                | x< -2.0 = (checkbound ((-1.999 - fx*0.01,y,z),(-0.3*fx,fy,fz)))
                                | z< -2.0 = (checkbound ((x,y,-1.999 - fz*0.01),(fx,fy,-0.3*fz)))
                                | y<0.0 = (checkbound ((x,0.0001 -fy*0.01,z),(fx,-0.1*fy,fz)))
                                | otherwise = ((x,y,z),(fx,fy,fz))

checkbounds :: [(Vec3,Vec3)] -> [(Vec3,Vec3)]
checkbounds pts = map checkbound pts


wconst:: GLfloat -> GLfloat
wconst h = 315 / (64*pi*h^9)

wpoly6 :: GLfloat -> GLfloat -> GLfloat
wpoly6 h r | r<h =  wconst h * (h^2-r^2)^3 -- (1 - r/h)^2 --  
           | otherwise = 0

wpoly6d:: GLfloat -> GLfloat -> GLfloat
wpoly6d h r | r<h =  wconst h * 6 * (h^2-r^2)^2
            | otherwise = 0

wpoly6dd:: GLfloat -> GLfloat -> GLfloat
wpoly6dd h r | r<h = wconst h * (h^2-r^2) * (4*r^2-(h^2-r^2))
             | otherwise = 0

dist :: Vec3 -> Vec3 -> GLfloat
dist (x,y,z) (w,v,h) = sqrt ( (x-w)^2 + (y-v)^2 + (z-h)^2 )

isnearby :: Vec3 -> Vec3 -> Bool
isnearby (a,b,c) (x,y,z) | ((a - x) == 0) && ((b - y) == 0) && ((c - z) == 0) = False
                         | ((abs (a - x)) < d) && ((abs (b - y)) < d) && ((abs (c - z)) < d) = True
                         | otherwise = False

dencity :: [(Vec3,Vec3)] -> (Vec3,Vec3) -> GLfloat
dencity (y:ys) (x,v) | isnearby x (fst y) = (m * (wpoly6 (dist x (fst y)) d)) + (dencity ys (x,v) )
                     | otherwise          = (dencity ys (x,v))
dencity [] _ = m

pressure :: [(Vec3,Vec3)] -> [GLfloat] -> (Vec3,Vec3) -> GLfloat -> Vec3 
pressure (y:ys) (dr:ds) (x,v) dc | isnearby x (fst y) = addp (mulp ( m * ( k * (dc + dr - 2 * douter) / (2 * dr) ) * (wpoly6d d (dist x (fst y)) )) (subp x (fst y))) (pressure ys ds (x,v) dc )
                                 | otherwise          = (pressure ys ds (x,v) dc)
pressure _ _ _ _ = (0,0,0)

viscosity :: [(Vec3,Vec3)] -> [GLfloat] -> (Vec3,Vec3) -> Vec3
viscosity (y:ys) (dr:ds) (x,v) | isnearby x (fst y) = addp (mulp (mu * m/dr * (wpoly6dd d (dist x (fst y)))) (subp (snd y) v)) (viscosity ys ds (x,v)) 
                               | otherwise          = (viscosity ys ds (x,v))
viscosity [] _ _ = (0,0,0)

tension :: [(Vec3,Vec3)] -> [GLfloat]  -> (Vec3,Vec3) -> Vec3
tension (y:ys) (dr:ds) (x,v) | isnearby x (fst y) = addp (mulp (sigma * m/dr * (wpoly6dd d (dist x (fst y)))) (subp (fst y) x)) (tension ys ds (x,v))
                             | otherwise          = (tension ys ds (x,v))
tension [] _ _ = (0,-9.8*m,0)

sumArr :: [Vec3]->[Vec3]->[Vec3]
sumArr (x:xs) (y:ys) = (addp x y):(sumArr xs ys)
sumArr _ _ = []

addForces :: GLfloat -> [(Vec3,Vec3)] -> [Vec3] -> [(Vec3,Vec3)]
addForces dt ((p,v):pts) (f:fcs) = (p, (addp v (mulp (dt/m) f))):(addForces dt pts fcs)
addForces _ _ _ = []


calcForces :: [(Vec3,Vec3)] -> [Vec3] 
calcForces pts = (let dens = (map (dencity pts) pts) in
                     sumArr 
                            (sumArr 
                                    (map (tension pts dens) pts)
                                    (map (viscosity pts dens) pts) 
                            ) 
                            (map2arg (pressure pts dens) pts dens) 
                 )

process :: GLfloat -> [(Vec3, Vec3)] -> [(Vec3, Vec3)]
process dt points = checkbounds (move (addForces dt points (calcForces points)) dt)