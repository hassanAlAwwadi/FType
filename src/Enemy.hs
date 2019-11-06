module Enemy where

import qualified Classess as C 
import Weapon(Gun, Bullet, simple, shoot, PowerUp)
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L


--import Weapon as W

data Enemy = Enemy{ size :: Float, pos :: Point, speed :: Float, direction :: Vector, health :: Float,   gun :: Gun, bullets :: [Bullet] } 
           | GraveMarker {size :: Float, pos :: Point, reward :: Maybe PowerUp}

enemy = Enemy{ 
    Enemy.size = 10, 
    pos = (10,10), 
    speed =5, 
    direction = (0,0), 
    health = 10, 
    gun = simple, 
    bullets = []
    }

instance C.Paint Enemy where
    paint Enemy{ size = s, pos = (x,y), bullets = b } = do
         pb <- C.paint b
         let pe = translate x y enemyDrawing 
         pure $ pictures [pe,pb] where 
         enemyDrawing = color red $ rectangleSolid s s 

instance C.Tick Enemy where
    --tick :: Float -> Enemy -> IO Enemy
    tick f e@Enemy{ pos = p, speed = v, direction = d, gun = g, bullets = b} = do
        tb <- C.tick f b
        ng <- C.tick f g
        let (ng, nb) = shoot  p (-1, 0)  g
        pure e { pos = p L.+ v L.* d, bullets = nb++b, gun = ng }
    
instance C.Collidable Enemy where
    --size :: Enemy ->  (Float,Float)
    size e = (Enemy.size e, Enemy.size e)
    --position :: Enemy -> (Float,Float)
    position e = pos e
    repos v e@Enemy{pos = p} = e{pos = p L.+ v} 
    reposChildren v e@Enemy{bullets = b} = e{bullets = map (C.reposWithChildren v) b}
