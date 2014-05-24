module Points where
 
import Graphics.UI.GLUT
 
points :: Int -> [((GLfloat,GLfloat,GLfloat), (GLfloat,GLfloat,GLfloat))]
points n = [ ((2*k/(n'-1)-5.0 ,2*l/(n'-1)+y, 2*m/(n'-1)-5.0), (0,0,0)) | k <- [0..(n'-1)], l <-[0..(n'-1)], m<-[0..(n'-1)], y<-[6..10] ]
   where n' = fromIntegral n