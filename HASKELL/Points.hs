module Points where
 
import Types
 
points :: Int -> [Particle]
points n = [ Particle ( 2 * k / (n' - 1) - 5.0, 
	                    2 * l / (n' - 1) + y,
	                    2 * m / (n' - 1) - 5.0 )
	                  (0, 0, 0) 
	                  (0::PositionT) 
	                  | k <- [0..(n'-1)]
	                  , l <-[0..(n'-1)]
	                  , m<-[0..(n'-1)]
	                  , y<-[6..15] ]
   where n' = fromIntegral n