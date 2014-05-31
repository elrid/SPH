module Bindings (idle, display, reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Data.IORef
import Display
import Types
 
reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef State -> KeyboardMouseCallback
keyboardMouse s key Down _ _ = case key of
  (SpecialKey KeyLeft ) -> s $~! \st -> st { camera = let (x, y) = (camera st) in (x, y+5) }
  (SpecialKey KeyRight) -> s $~! \st -> st { camera = let (x, y) = (camera st) in (x, y-5) }
  (SpecialKey KeyUp   ) -> s $~! \st -> st { camera = let (x, y) = (camera st) in (x/1.2, y) }
  (SpecialKey KeyDown ) -> s $~! \st -> st { camera = let (x, y) = (camera st) in (x*1.2, y) }
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()