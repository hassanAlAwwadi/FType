module Ship where

import Classess as C
import Graphics.Gloss
import Weapon(simple, Gun,Bullet, cooldown, shoot)
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
  timer :: Float,
  size :: (Float,Float)
  }

ship = Ship ((-420),0) 5 (0,0) simple [] 0 0 (40,20) 

instance Paint Ship where
    paint (Ship (x,y) v (dx,dy) _ b _ _ (sx,sy)) = do 
      pb <- paint b
      let ps = translate x y shipDrawing 
      pure $ pictures [ps,pb] where 
      shipDrawing = color blue $ rectangleSolid sx sy 

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
    tick f s@(Ship p v d g bs _ t _)  = do 
        let np =  p L.+ v L.* d
        tb <- tick f bs
        let (nb, nt) = if t + f > cooldown g
            then (shoot g np (1,0) ++ tb, 0)
            else (tb, t + f)
        return s {bullets = nb, timer = nt, pos = np }

instance Collidable Ship where
    --size :: Ship ->  (Float,Float)
    size s = Ship.size s 
    --position :: Ship -> (Float,Float)
    position s = pos s 

