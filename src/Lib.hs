{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( someFunc
      ,GameState
    ) where

import System.IO (readFile)
import Data.List (sortBy)
import qualified Data.Ord as D (Down(..), comparing) 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Classess as C
import World
import Menu

someFunc :: IO ()
someFunc = playIO FullScreen black 30 (MainMenu mainMenu) paint handle tick

data WorldState = Paused WorldState | Scrolling Float | BossFight deriving (Show)
pause (Paused s) = s
pause  s         = Paused s

data GameState = Playing World WorldState | MainMenu (Menu GameState) | HighScores (IO [(String, Float)])

mainMenu = createMenu [("Level 1", loadLevel1), ("HighScore", loadHighScore)]
    

instance Paint GameState where
  paint (MainMenu m) = paint m
  paint (HighScores ioScores) = do
    scores <- ioScores
    let nameandscores = map (\(name, score) -> Text $ name ++ ":" ++ show score) scores
    let translated = translate 0 <$> [0, (-200)..] <*> nameandscores
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
    handle (EventKey (SpecialKey KeyEnter) Down _ _) g@(MainMenu m) = pure $ menuAction m g 
    handle e (MainMenu m)  = do 
        nm <- handle e m
        return $ MainMenu nm
    handle e (Playing w s) = do 
        nw <- handle e w
        return $ Playing nw s
    handle e p = return p
 
loadLevel1 _ = Playing world (Scrolling (-0.5))
loadHighScore _ = 
    let scores = do 
            file <- readOrCreateFile  "HighScores.txt" 
            let filelines = lines file
            let scores = sortBy (D.comparing $ D.Down . snd) $ map read filelines
            return scores
    in HighScores scores

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


