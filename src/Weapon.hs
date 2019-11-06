{-# LANGUAGE DuplicateRecordFields #-}
module Weapon(PowerUp(..), Gun, Bullet, shoot, simple) where

import Classess
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L

data Gun = Simple     { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, speed :: Float } 
         | SpreadShot { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, speed :: Float,  amount :: Int, angle :: Int } 
         | Laser      { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, livespan :: Float} 

instance Tick Gun where
    tick f g = pure $ g{countdown = countdown g - f}

-- | the "level 1" specs of the three weapon types
simple, spreadShot, laser :: Gun
simple     = Simple     {cal = 10, power = 1, speed = 1, cooldown = 0.5, countdown = 0.5 }
spreadShot = SpreadShot {cal = 10, power = 1, speed = 1, cooldown = 1  , countdown = 1  , amount = 3, angle = 15 }
laser      = Laser      {cal = 10, power = 1, cooldown = 3  , countdown = 3  , livespan = 3 }


data Bullet = Bullet { size :: Float, pos :: Point, speed :: Float, direction :: Vector}

instance Paint Bullet where
    paint (Bullet s (x,y) _ _) = pure $ translate x y bulletDrawing where 
      bulletDrawing = color yellow $ circleSolid s

instance Tick Bullet where
    --tick :: Float -> Bullet -> IO Bullet
    tick f b@(Bullet _ p v d)  = pure b{ pos = p L.+ v L.* d}

instance Collidable Bullet where
    --size :: Bullet ->  (Float,Float)
    size b = (Weapon.size b, Weapon.size b)
    --position :: Bullet -> (Float,Float)
    position = pos
    repos v b@Bullet{pos = p} = b{pos = p L.+ v}
    reposChildren v = id

-- | the only exported way to produce a bullet
shoot :: Point -> Vector -> Gun -> (Gun , [Bullet])
shoot  p d g@Simple{ cal = c, speed = s, countdown = count, cooldown = cool } 
    | count <= 0 = (g{countdown = cool}, [Bullet { size = c, pos = p, speed = s, direction = d}])
    | otherwise = (g, [])

-- | the power ups used to make a gun stronger
data PowerUp = PUp      -- Simple power up
             | SUp      -- Spreadshot power up
             | LUp      -- Laser power up
              deriving(Show)

instance Paint PowerUp where
    paint p = pure . Text $ show p

-- power up your gun when you pick up a upgrade
powerUp :: Gun -> PowerUp -> Gun
powerUp g@Simple    { power = p}               PUp = g{ power = p * 1.2 }
powerUp g@SpreadShot{ power = p, amount = a}   SUp = g{ power = p * 1.05, amount = a + 1 }
powerUp g@Laser    { power = p, livespan = l } LUp = g{ power = p * 1.3, livespan = l + 0.3 }
-- gun and powerup don't match -> gun gets reset
powerUp _  PUp = simple
powerUp _  SUp = spreadShot
powerUp _  LUp = laser