{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( someFunc
    ) where

import System.IO (readFile)
import Data.List (sortBy)
import qualified Data.Ord as D (Down(..), comparing) 
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

data WorldState = Paused WorldState | Scrolling Float | BossFight deriving (Show)
pause (Paused s) = s
pause  s         = Paused s

data GameState = Playing World WorldState | Menu Int | HighScores (IO [(String, Float)])

instance Paint GameState where
  paint (Menu x)  = pure $ pictures [mainMenu, translate 0 (fromIntegral $ x * (-250)) menuSelector]  where 
    mainMenu =  translate (-50) 250 $ 
                pictures [                      color white $ Text "Level 1",
                           translate 0 (-250) $ color white $ Text "High scores"
                         ]
    menuSelector = translate 350 300 $ color yellow $ rectangleWire 800 200
  paint (HighScores ioScores) = do
    scores <- ioScores
    let nameandscores = map (\(name, score) -> Text $ name ++ ":" ++ show score) scores
    let translated = fmap (\(tr, te) -> translate 0  tr te) $ zip [0, (-200)..] nameandscores
    let single = color white $ pictures translated
    return $ translate (-900) 300 single
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
    handle e p = return p
 
menuAction 0 = Playing world (Scrolling (-0.5))
menuAction 1 = 
    let scores = do 
            file <- readOrCreateFile  "HighScores.txt" 
            let filelines = lines file
            let scores = sortBy (D.comparing $ D.Down . snd) $ map read filelines
            return scores
    in HighScores scores
menuAction n = Menu n

readOrCreateFile :: FilePath -> IO String
readOrCreateFile p = do 
    nothing <- appendFile p ""
    readFile p 


  
instance Tick GameState where
    tick f g@(Playing _ (Paused _)) = pure g
    tick f (Playing w s@BossFight)            = do 
        nw <- tick f w
        return $ Playing nw s
    tick f (Playing w (Scrolling h))            = do 
        nw <-  tick f $ scroll h w
        return $ Playing nw (Scrolling h)
    tick _ a                        = pure a 


