{-# LANGUAGE DuplicateRecordFields #-}
module Weapon where

import Classess
import Graphics.Gloss

data Gun = Simple { cal :: Float, speed :: Float } 
         | SpreadShot { amount :: Int, angle :: Int, cal :: Float, speed :: Float } 
         | Laser { width :: Int, power :: Int } 

data Bullet = Bullet { size :: Float, pos :: Point, speed :: Float, direction :: Vector}

instance Paint Bullet where
    paint (Bullet s (x,y) _ _) = pure $ translate x y bulletDrawing where 
      bulletDrawing = color yellow $ circleSolid s
