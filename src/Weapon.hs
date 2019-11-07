{-# LANGUAGE DuplicateRecordFields #-}
module Weapon(PowerUp(..), Gun, Bullet, shoot, simple, dmg, powerUp, randomPowerUp) where

import Classess
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L

data Gun = Simple     { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, speed :: Float } 
         | SpreadShot { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, speed :: Float,  amount :: Int, angle :: Int } 
         | Laser      { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, lifespan :: Float} 

instance Tick Gun where
    tick f g = pure $ g{countdown = countdown g - f}

-- | the "level 1" specs of the three weapon types
simple, spreadShot, laser :: Gun
simple     = Simple     {cal = 10, power = 1, speed = 5, cooldown = 0.5, countdown = 0.5 }
spreadShot = SpreadShot {cal = 10, power = 1, speed = 5, cooldown = 1  , countdown = 1  , amount = 3, angle = 15 }
laser      = Laser      {cal = 10, power = 1, cooldown = 3  , countdown = 3  , lifespan = 0.5 }


data Bullet = Bullet    { size :: Float , pos :: Point, speed :: Float, direction :: Vector, dmg :: Float } 
            | LaserBeam { width :: Float, pos :: Point, dmg :: Float, lifespan :: Float }

instance Paint Bullet where
    paint Bullet{ size = s, pos = (x,y) }     = pure $ translate x y . color yellow $ circleSolid s where 
    paint LaserBeam{ width = w, pos = (x,y) } = pure $ translate x y . color yellow $ rectangleSolid 4000 w

instance Tick Bullet where
    --tick :: Float -> Bullet -> IO Bullet
    tick _ b@Bullet{pos = p, speed = v, direction = d}  = pure (b{ pos = p L.+ v L.* d}::Bullet)
    tick f b@LaserBeam{lifespan = l} = pure (b{lifespan = l - f}::Bullet)

instance Collidable Bullet where
    --size :: Bullet ->  (Float,Float)
    size b = (Weapon.size b, Weapon.size b)
    --position :: Bullet -> (Float,Float)
    position = pos
    repos v b@Bullet{pos = p} = b{pos = p L.+ v}
    repos _ l@LaserBeam{}     = l
    reposChildren _ = id

-- | the only exported way to produce a bullet
shoot :: Point -> Vector -> Gun -> (Gun , [Bullet])
shoot  p d g
    | countdown g <= 0 = (g{countdown = cooldown g}, shoot' p d g)
    | otherwise  = (g, [])

shoot' :: Point -> Vector -> Gun -> [Bullet]
shoot'  p d Simple    { cal = c, speed = s, power = pw }  = [Bullet { size = c, pos = p, speed = s, direction = d, dmg = pw } ]
shoot'  p d SpreadShot{ cal = c, speed = s, power = pw }  = [Bullet { size = c, pos = p, speed = s, direction = d, dmg = pw } ]
shoot'  p _ Laser     { cal = c, power = pw, lifespan = l }  = [LaserBeam{ width = c, pos = p L.+ (2000,0), dmg = pw, lifespan = l} ]

-- | the power ups used to make a gun stronger
data PowerUp = PUp { pos :: Point } -- Simple power up
             | SUp { pos :: Point } -- Spreadshot power up
             | LUp { pos :: Point } -- Laser power up
              deriving(Show)

instance Paint PowerUp where
    paint p = pure . color blue . scale 0.5 0.5 . Text $ case p of 
        PUp{} -> "LUp"
        SUp{} -> "LUp"
        LUp{} -> "LUp"

instance Collidable PowerUp where
    size _ = (1,1)
    position = pos
    repos l p = p{pos = l}
    reposChildren _ = id

randomPowerUp :: Int -> Point -> Maybe PowerUp
randomPowerUp randomVal point
    | randomVal <= 5  = Just $ PUp point
    | randomVal <= 10 = Just $ SUp point
    | randomVal <= 15 = Just $ LUp point
    | otherwise       = Nothing

-- power up your gun when you pick up a upgrade
powerUp :: Gun -> PowerUp -> Gun
powerUp g@Simple    { power = p}                PUp{} = g{ power = p * 1.2 }
powerUp g@SpreadShot{ power = p, amount = a}    SUp{} = g{ power = p * 1.05, amount = a + 1 }
powerUp g@Laser     { power = p, lifespan = l } LUp{} = g{ power = p * 1.3, lifespan = l + 0.1 }
-- gun and powerup don't match -> gun gets reset
powerUp _  PUp{} = simple
powerUp _  SUp{} = spreadShot
powerUp _  LUp{} = laser