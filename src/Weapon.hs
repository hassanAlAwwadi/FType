{-# LANGUAGE DuplicateRecordFields #-}
module Weapon where

import Classess
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L


data Gun = Simple { cal :: Float, speed :: Float, cooldown :: Float } 
         | SpreadShot { amount :: Int, angle :: Int, cal :: Float, speed :: Float, cooldown :: Float } 
         | Laser { width :: Int, power :: Int , cooldown :: Float} 

simple = Simple {cal = 20, speed = 1, cooldown = 0.5}
data Bullet = Bullet { size :: Float, pos :: Point, speed :: Float, direction :: Vector}

instance Paint Bullet where
    paint (Bullet s (x,y) _ _) = pure $ translate x y bulletDrawing where 
      bulletDrawing = color yellow $ circleSolid s

instance Tick Bullet where
    --tick :: Float -> Bullet -> IO Bullet
    tick f b@(Bullet _ p v d)  = pure $ (b { pos = p L.+ v L.* d})
    
bullet = Bullet { size = 1000, pos = (10,10), speed = 10, direction = (2,2)}

shoot :: Gun -> Point -> Vector -> Bullet
shoot (Simple c s _) p d = Bullet { size = c, pos = p, speed = s, direction = d}

instance Collidable Bullet where
    --size :: Bullet ->  (Float,Float)
    size b = (Weapon.size b, Weapon.size b)
    --position :: Bullet -> (Float,Float)
    position b = pos b 