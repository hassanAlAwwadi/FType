{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( someFunc
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Classess as C
import Ship
import Weapon
import Enemy
import World

someFunc :: IO ()
someFunc = playIO FullScreen black 30 (Menu 0) paint handle tick

data WorldState = Paused WorldState | Scrolling | BossFight deriving (Show)
pause (Paused s) = s
pause  s         = Paused s

data GameState = Playing World WorldState | Menu Int

instance Paint GameState where
  paint (Menu x)  = pure $ pictures [mainMenu, translate 0 (fromIntegral $ x * (-250)) menuSelector]  where 
    mainMenu = pictures [translate (-50) 250 $ color white $ Text "Level 1",
                         translate (-50) 0 $ color white $ Text    "High scores"]
    menuSelector = translate 350 300 $ color yellow $ rectangleWire 800 200
  paint (Playing w (Paused s)) = do 
    pw <- paint w
    let pp = translate (-900) (-100) $ scale 4 4 $ color white $ text $ show (Paused s)
    pure $ pictures [pw, pp]
  paint (Playing w _) = paint w



instance Handle GameState where 
    handle (EventKey (SpecialKey KeyEsc)   Down _ _) _  = exitSuccess
    handle (EventKey (Char 'p') Down _ _) (Playing w s) = pure $ Playing w $ pause s
    handle e (Menu n) = pure $ case e of 
      EventKey (Char 'w')            Down _ _ -> Menu (max 0 $ n-1)
      EventKey (Char 's')            Down _ _ -> Menu (min 1 $ n+1)
      EventKey (SpecialKey KeyEnter) Down _ _ -> menuAction n 
      _                                       -> Menu n
    handle e (Playing w s)   = do nw <- handle e w
                                  return $ Playing nw s
 
menuAction 0 = Playing world Scrolling where 
    world = World {
        player = ship,
        enemies = [Enemy { size = 10, pos = (10,10), speed =5, direction = (0,0), health = 10, gun = simple, bullets = [], timer = 0} ],
        lives = 3,
        score = 0,
        level = 0,
        timer = 0
    } 
menuAction n = Menu n
  
instance Tick GameState where
    tick f g@(Playing _ (Paused _)) = pure g
    tick f (Playing w s)            = do 
        nw <- tick f w
        return $ Playing nw s
    tick _ a                        = pure a 



