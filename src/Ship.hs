module Ship where

import Classess
import Graphics.Gloss
import Weapon(simple, Gun,Bullet)
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as L


data Ship = Ship 
  {
  pos :: Point, 
  speed :: Float,
  direction :: Vector,
  gun :: Gun, 
  bullets :: [Bullet],
  bombs :: Int,
  timer :: Float
  } 

ship = Ship (0,0) 5 (0,0) (Simple 10 10) [Bullet { size = 10, pos = (10,10), speed = 10, direction = (2,2)}] 1 

instance Paint Ship where
    paint (Ship (x,y) v (dx,dy) _ b _) = do 
      pb <- paint b
      let ps = translate x y shipDrawing 
      pure $ pictures [ps,pb] where 
      shipDrawing = color blue $ rectangleSolid 40 20 

instance Handle Ship where
    --handle :: Event -> Ship -> IO Ship
    handle e s = pure $ case e of 
        EventKey (Char 'w') Down _ _ -> s { direction = ( fst $ direction s,  1 ) }
        EventKey (Char 's') Down _ _ -> s { direction = ( fst $ direction s, -1 ) }
        EventKey (Char 'd') Down _ _ -> s { direction = (  1, snd $ direction s ) }
        EventKey (Char 'a') Down _ _ -> s { direction = ( -1, snd $ direction s ) }
        EventKey (Char 'w') Up   _ _ -> s { direction = ( fst $ direction s,  0 ) }
        EventKey (Char 's') Up   _ _ -> s { direction = ( fst $ direction s,  0 ) }
        EventKey (Char 'd') Up   _ _ -> s { direction = (  0, snd $ direction s ) }
        EventKey (Char 'a') Up   _ _ -> s { direction = (  0, snd $ direction s ) }
        _                            -> s

instance Tick Ship where
    --tick :: Float -> Ship -> IO Ship
    tick f s@(Ship p v d _ _ _ t)  = pure $ (s { pos = p L.+ v L.* d, timer = t + f} :: Ship)
