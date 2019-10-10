{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( someFunc
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as L
import System.Exit

someFunc :: IO ()
someFunc = playIO FullScreen black 30 (Menu 0) paint handle tick

class Paint p where
    paint :: p -> IO Picture

instance Paint p => Paint [p] where
    paint [] = return blank 
    paint ps = do 
        paintings <- sequence $ map paint ps
        return $ pictures paintings

class Handle h where
    handle :: Event -> h -> IO h

instance Handle h => Handle [h] where 
    handle _ [] = return []
    handle e hs = sequence $ map (handle e) hs

class Tick t where
    tick :: Float -> t -> IO t
instance Tick h => Tick [h] where 
    tick f [] = return []
    tick f hs = sequence $ map (tick f) hs


data GameState = Paused World | Scrolling World | BossFight World | Menu Int

instance Paint GameState where
  paint (Menu x)  = pure $ pictures [mainMenu, translate 0 (fromIntegral $ x * (-250)) menuSelector]  where 
    mainMenu = pictures [translate (-50) 250 $ color white $ Text "Level 1",
                         translate (-50) 0 $ color white $ Text    "High scores"]
    menuSelector = translate (350) 300 $ color yellow $ rectangleWire 800 200
  paint (Scrolling w) = paint w

instance Handle GameState where 
    handle (EventKey (SpecialKey KeyEsc)   Down _ _) _ = exitWith ExitSuccess
    handle e (Menu n) = pure $ case e of 
      EventKey (Char 'w')            Down _ _ -> Menu (max 0 $ n-1)
      EventKey (Char 's')            Down _ _ -> Menu (min 1 $ n+1)
      EventKey (SpecialKey KeyEnter) Down _ _ -> case n of 
                                            0 -> getLevel 1
                                            _ -> Menu n
      _                                       -> Menu n
    handle e (Scrolling w) =  do nw <- handle e w
                                 return $ Scrolling nw
    handle _ g = pure g
 
getLevel 1 = Scrolling $ World {
    player = baseShip,
    pbullets = [],
    enemies = [],
    ebullets = [],
    lives = 3,
    score = 0,
    level = 0
    } where 
      baseShip    = Ship (0,0) 5 (0,0) (Simple 10 10) 1 
  
instance Tick GameState where
    tick f (Scrolling w) = do 
        nw <- tick f w
        return $ Scrolling nw
    tick _ a = pure a 

data World = World 
  {
  player :: Ship, 
  pbullets :: [Bullet],
  enemies :: [Enemy],
  ebullets :: [Bullet],
  lives :: Int, 
  score :: Int,
  level :: Int 
  }

instance Paint World where
    paint w = paint $ player w

instance Handle World where
    handle e w = do 
        p <- handle e $ player w
        return w {player = p}

instance Tick World where 
    tick f w = do 
        p <- tick f $ player w
        return w {player = p}

data Ship = Ship 
  {
  pos :: Point, 
  speed :: Float,
  direction :: Vector,
  bulletType :: Bullet, 
  bombs :: Int
  } deriving (Show)

instance Paint Ship where
    paint (Ship (x,y) v (dx,dy) _ _) = pure $ translate x y shipDrawing where 
      shipDrawing = color blue $ rectangleSolid 40 20 

instance Handle Ship where
    handle e s = pure $ case e of 
        EventKey (Char 'w') Down _ _ -> s { direction = ( fst $ direction s,  1 ) }
        EventKey (Char 's') Down _ _ -> s { direction = ( fst $ direction s, -1 ) }
        EventKey (Char 'd') Down _ _ -> s { direction = (  1, snd $ direction s ) }
        EventKey (Char 'a') Down _ _ -> s { direction = ( -1, snd $ direction s ) }
        EventKey (Char 'w') Up   _ _ -> s { direction = ( fst $ direction s,  0 ) }
        EventKey (Char 's') Up   _ _ -> s { direction = ( fst $ direction s,  0 ) }
        EventKey (Char 'd') Up   _ _ -> s { direction = (  0, snd $ direction s ) }
        EventKey (Char 'a') Up   _ _ -> s { direction = (  0, snd $ direction s ) }
        _                            -> s

instance Tick Ship where
    tick :: Float -> Ship -> IO Ship
    tick f s@(Ship p v d _ _)  = pure $ s { pos = p L.+ v L.* d} 

data Enemy = Enemy { pos :: Point, size :: Float, speed :: Double, health :: Double, bulletType :: Bullet,} 

instance Paint Enemey where
    paint (Enemy (x,y) s _ _) = pure $ translate x y enemyDrawing where 
        enemyDrawing = color red $ rectangleSolid s s 

data Bullet
  = Simple { size :: Float, speed :: Float } 
  | SpreadShot { amount :: Int, angle :: Int } 
  | Laser { width :: Int, power :: Int } 
  deriving (Show)
