{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( someFunc
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Classess
import Ship
import Weapon
import Enemy

someFunc :: IO ()
someFunc = playIO FullScreen black 30 (Menu 0) paint handle tick

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
    enemies = [],
    lives = 3,
    score = 0,
    level = 0
    } where 
      baseShip    = Ship (0,0) 5 (0,0) (Simple 10 10) [] 1 
  
instance Tick GameState where
    tick f (Scrolling w) = do 
        nw <- tick f w
        return $ Scrolling nw
    tick _ a = pure a 

data World = World 
  {
  player :: Ship, 
  enemies :: [Enemy],
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

