module Enemy where

import Classess
import Weapon
import Graphics.Gloss

--import Weapon as W

data Enemy = Enemy { size :: Float, pos :: Point, speed :: Float, direction :: Vector, health :: Float,   gun :: Gun, bullets :: [Bullet] } 

instance Paint Enemy where
    paint (Enemy s (x,y) _ _ _ _  b ) = do
         pb <- paint b
         let pe = translate x y enemyDrawing 
         pure $ pictures [pe,pb] where 
         enemyDrawing = color red $ rectangleSolid s s 