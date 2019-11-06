module Ship where

import qualified Classess as C
import Graphics.Gloss
import Weapon(simple, Gun,Bullet, shoot)
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as L


data Ship = Ship {
    pos :: Point, 
    speed :: Float,
    direction :: Vector,
    gun :: Gun, 
    bullets :: [Bullet],
    bombs :: Int,
    size :: (Float,Float)
    }

ship = Ship{ 
    pos = (-420,0), 
    speed = 5, 
    direction = (0,0), 
    gun = simple, 
    bullets = [], 
    bombs = 0, 
    size = (40,20)
    } 

instance C.Paint Ship where
    paint Ship{ pos = (x,y), speed = v, direction = (dx,dy), bullets = b, size = (sx,sy) } = do 
        pb <- C.paint b
        let ps = translate x y shipDrawing 
        pure $ pictures [ps,pb] where 
        shipDrawing = color blue $ rectangleSolid sx sy 

instance C.Handle Ship where
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

instance C.Tick Ship where
    --tick :: Float -> Ship -> IO Ship
    tick f s@(Ship p v d g bs _ _)  = do 
        let np =  p L.+ v L.* d
        tb <- C.tick f bs
        tg <- C.tick f g
        let (ng', nb) = shoot np (1,0) tg
        return s {bullets = nb ++ tb, pos = np, gun = ng' }

instance C.Collidable Ship where
    --size :: Ship ->  (Float,Float)
    size = size
    --position :: Ship -> (Float,Float)
    position = pos 
    repos v s@Ship{ pos = p } = s{ pos = p L.+ v } 
    reposChildren v s@Ship{ bullets = b } = s{ bullets = map (C.reposWithChildren v) b }



