module Ship(Ship, bullets, Ship.powerUp, pos) where

import qualified Classess as C
import Graphics.Gloss
import Weapon as W(Gun,Bullet, PowerUp, simple, shoot, powerUp) 
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
} deriving (Read, Show)

instance C.Creatable Ship where
    create _ _ = Ship{ 
        pos = (-420,0), 
        speed = 13, 
        direction = (0,0), 
        gun = simple, 
        bullets = [], 
        bombs = 0, 
        size = (40,20)
    } 

powerUp :: Ship -> PowerUp -> Ship
powerUp s p = s{ gun = W.powerUp (gun s) p}

instance C.Paint Ship where
    paint Ship{ pos = (x,y), bullets = b, size = (sx,sy) } =  
        let pb = C.paint b
            ps = translate x y shipDrawing 
        in pictures [ps,pb] where 
        shipDrawing = color blue $ rectangleSolid sx sy 

instance C.Handle Ship where
    --handle :: Event -> Ship -> IO Ship
    handle e s = case e of 
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
    tick f s@(Ship p v d g bs _ _)  = let 
        np =  p L.+ v L.* d
        tb = C.tick f bs
        tg = C.tick f g
        (ng', nb) = shoot np (1,0) tg
        in s {bullets = nb ++ tb, pos = np, gun = ng' }

instance C.Collidable Ship where
    --size :: Ship ->  (Float,Float)
    size = size
    --position :: Ship -> (Float,Float)
    position = pos 
    repos v s@Ship{ pos = p } = s{ pos = p L.+ v } 
    reposChildren v s@Ship{ bullets = b } = s{ bullets = map (C.reposWithChildren v) b }