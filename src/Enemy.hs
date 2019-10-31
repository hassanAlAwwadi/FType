module Enemy where

import Classess as C 
import Weapon(Gun, Bullet, simple, shoot, cooldown)
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L


--import Weapon as W

data Enemy = Enemy { size :: Float, pos :: Point, speed :: Float, direction :: Vector, health :: Float,   gun :: Gun, bullets :: [Bullet], timer :: Float } 
enemy = Enemy { Enemy.size = 10, pos = (10,10), speed =5, direction = (0,0), health = 10, gun = simple, bullets = [], timer = 0}

instance Paint Enemy where
    paint (Enemy s (x,y) _ _ _ _  b _) = do
         pb <- paint b
         let pe = translate x y enemyDrawing 
         pure $ pictures [pe,pb] where 
         enemyDrawing = color red $ rectangleSolid s s 

instance Tick Enemy where
    --tick :: Float -> Enemy -> IO Enemy
    tick f e@(Enemy _ p v d _ g b t) = do
        tb <- tick f b
        let (nb, nt) = if t + f > cooldown g
            then (shoot g p ((-1), 0) ++ tb, 0)
            else (tb, t + f)
        let te = (e { pos = p L.+ v L.* d, bullets = nb, timer = nt})
        pure te
    
instance Collidable Enemy where
    --size :: Enemy ->  (Float,Float)
    size e = (Enemy.size e, Enemy.size e)
    --position :: Enemy -> (Float,Float)
    position e = pos e
    repos v e@Enemy{pos = p} = e{pos = p L.+ v} 
    reposChildren v e@Enemy{bullets = b} = e{bullets = map (reposWithChildren v) b}
