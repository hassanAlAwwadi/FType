{-# LANGUAGE DuplicateRecordFields #-}
module Weapon(PowerUp(..), Gun(..) , Bullet, shoot, simple, spreadShot, dmg, powerUp, randomPowerUp) where

import Classess
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L

data Gun = Simple     { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, speed :: Float } 
        -- | amount represents the amount of sideshots. so 1 -> 3 bullets total. 2 -> 5 bullets total. 3 -> 7 bullets total 
         | SpreadShot { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, speed :: Float,  amount :: Int, angle :: Float } 
        -- WIP: A lot different from the other two guns.
        -- Laser      { cal :: Float, power :: Float, cooldown :: Float, countdown :: Float, lifespan :: Float} 
         deriving (Read, Show)

instance Tick Gun where
    tick f g = g{countdown = countdown g - f}

-- | the "level 1" specs of the three weapon types
simple, spreadShot :: Gun
simple     = Simple     {cal = 5, power = 1, speed = 8, cooldown = 1, countdown = 1 }
spreadShot = SpreadShot {cal = 7, power = 1, speed = 8, cooldown = 1  , countdown = 1  , amount = 1, angle = 0.3 }


data Bullet = Bullet    { size :: Float , pos :: Point, speed :: Float, direction :: Vector, dmg :: Float } deriving(Show, Read)

instance Paint Bullet where
    paint Bullet{ size = s, pos = (x,y) }     = translate x y . color yellow $ circleSolid s 

instance Tick Bullet where
    --tick :: Float -> Bullet -> IO Bullet
    tick _ b@Bullet{pos = p, speed = v, direction = d}  =  b{ pos = p L.+ v L.* d}

instance Collidable Bullet where
    --size :: Bullet ->  (Float,Float)
    size b = (Weapon.size b, Weapon.size b)
    --position :: Bullet -> (Float,Float)
    position = pos
    repos v b@Bullet{pos = p} = b{pos = p L.+ v}
    reposChildren _ = id

-- | the only exported way to produce a bullet
shoot :: Point -> Vector -> Gun -> (Gun , [Bullet])
shoot  p d g
    | countdown g <= 0 = (g{countdown = cooldown g}, shoot' p d g)
    | otherwise  = (g, [])

shoot' :: Point -> Vector -> Gun -> [Bullet]
shoot'  p d Simple    { cal = c, speed = s, power = pw }  = [Bullet { size = c, pos = p, speed = s, direction = d, dmg = pw } ]
shoot'  p d@(x,y) SpreadShot{ cal = c, speed = s, power = pw, amount = am, angle = an } = [ Bullet { size = c, pos = p, speed = s, direction = (x,y + fromIntegral d *  an ), dmg = pw }   | d <- [-am..am]]
                                                                
data PowerUp = PUp { pos :: Point } -- Simple power up
             | SUp { pos :: Point } -- Spreadshot power up
              deriving(Show, Read)


instance Paint PowerUp where
    paint p = translate x y . color blue . scale 0.3 0.3 . Text $ t where 
        (t,(x,y)) = case p of 
            PUp{pos = xy} -> ("PUp",xy)
            SUp{pos = xy} -> ("SUp",xy)

instance Collidable PowerUp where
    size _ = (60,60)
    position = pos
    repos l p = p{pos = l}
    reposChildren _ = id

randomPowerUp :: Int -> Point -> Maybe PowerUp
randomPowerUp randomVal point
    | randomVal <= 20 = Just $ PUp point
    | randomVal <= 40 = Just $ SUp point
    | otherwise       = Nothing

-- power up your gun when you pick up a upgrade
powerUp :: Gun -> PowerUp -> Gun
powerUp g@Simple    { power = p , speed = s, cal = c, cooldown = co}  PUp{} = g{ power = p * 1.5 , speed = s*1.3 , cal = c*1.2, cooldown = co*0.8 }
powerUp g@SpreadShot{ power = p, amount = a}    SUp{}                       = g{ power = p * 1.05, amount = a + 1, angle = 0.9/ fromIntegral a}
-- gun and powerup don't match -> gun gets reset
powerUp _  PUp{} = simple
powerUp _  SUp{} = spreadShot
