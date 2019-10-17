{-# LANGUAGE DuplicateRecordFields #-}
module Weapon where

import Graphics.Gloss

data Gun = Simple { cal :: Float, speed :: Float, cooldown :: Float } 
         | SpreadShot { amount :: Int, angle :: Int, cal :: Float, speed :: Float, cooldown :: Float } 
         | Laser { width :: Int, power :: Int , cooldown :: Float} 

simple = Simple {cal = 1, speed = 1, cooldown = 0.5}
data Bullet = Bullet { size :: Float, pos :: Point, speed :: Float, direction :: Vector}