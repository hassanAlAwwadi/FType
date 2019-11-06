{-# LANGUAGE DuplicateRecordFields #-}
module Weapon(PowerUp, Gun, Bullet, shoot, simple) where

import Classess
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L


data PowerUp = PUp      -- Simple power up
             | SUp      -- Spreadshot power up
             | LUp      -- Laser power up

data Gun = Simple     { cal :: Float, speed :: Float, cooldown :: Float, countdown :: Float} 
         | SpreadShot { cal :: Float, speed :: Float, cooldown :: Float, countdown :: Float, amount :: Int, angle :: Int } 
         | Laser      { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, livespan :: Float} 

simple = Simple {cal = 10, speed = 1, cooldown = 0.5, countdown = 0.5}

instance Tick Gun where
    tick f g = pure $ g{countdown = countdown g - f}

data Bullet = Bullet { size :: Float, pos :: Point, speed :: Float, direction :: Vector}

instance Paint Bullet where
    paint (Bullet s (x,y) _ _) = pure $ translate x y bulletDrawing where 
      bulletDrawing = color yellow $ circleSolid s

instance Tick Bullet where
    --tick :: Float -> Bullet -> IO Bullet
    tick f b@(Bullet _ p v d)  = pure $ (b { pos = p L.+ v L.* d})

instance Collidable Bullet where
    --size :: Bullet ->  (Float,Float)
    size b = (Weapon.size b, Weapon.size b)
    --position :: Bullet -> (Float,Float)
    position = pos
    repos v b@Bullet{pos = p} = b{pos = p L.+ v}
    reposChildren v = id
shoot :: Point -> Vector -> Gun -> (Gun , [Bullet])
shoot  p d g@Simple{cal = c, speed = s, countdown = count, cooldown = cool} 
    | count <= 0 = (g{countdown = cool}, [Bullet { size = c, pos = p, speed = s, direction = d}])
    | otherwise = (g, [])