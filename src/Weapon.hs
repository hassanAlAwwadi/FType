{-# LANGUAGE DuplicateRecordFields #-}
module Weapon where

import Graphics.Gloss

data Gun = Simple { cal :: Float, speed :: Float } 
         | SpreadShot { amount :: Int, angle :: Int, cal :: Float, speed :: Float } 
         | Laser { width :: Int, power :: Int } 

data Bullet = Bullet { size :: Float, pos :: Point, speed :: Float, direction :: Vector}