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

data WorldState = Paused { past ::WorldState } | Scrolling {scrollSpeed :: Float} | BossFight deriving (Show)
pause (Paused s) = s
pause  s         = Paused s

data GameState = Playing    { game :: World, state :: WorldState} 
               | MainMenu   { menu ::Menu GameState} 
               | HighScores { mvps :: IO [(String, Float)]}

--smart constructor of menu is used to create the main menu
mainMenu = createMenu [("Level 1", loadLevel1), ("HighScore", loadHighScore)]

--level 1 is wip, ok? OK?
loadLevel1 _ = Playing world (Scrolling (-0.5))

--loading the highscores
loadHighScore _ = 
    let scores = do 
            file <- readOrCreateFile  "HighScores.txt" 
            let filelines = lines file
            let scores = sortBy (D.comparing $ D.Down . snd) $ map read filelines
            return scores
    in HighScores scores

--crashing the game is ugly so this makes sure that the game survives, even if the highscore file wasn't created
readOrCreateFile :: FilePath -> IO String
readOrCreateFile p = do 
    nothing <- appendFile p ""
    readFile p 

instance Paint GameState where
    --paint menu
    paint MainMenu{menu = m}         = paint m
    --paint highscores
    paint HighScores{mvps = ioNames} = do
        scores <- ioNames
        let nameandscores = map (\(name, score) -> Text $ name ++ ":" ++ show score) scores
        let translated = translate 0 <$> [0, (-200)..] <*> nameandscores
        let single = color white $ pictures translated
        return $ translate (-900) 300 single
    --paint paused game
    paint Playing{game = w, state = (Paused s)} = do 
        pw <- paint w
        let pauseScreen = translate (-900) (-100) $ scale 4 4 $ color white $ text $ show (Paused s)
        pure $ pictures [pw, pauseScreen]
    --paint game
    paint Playing{game = w} = paint w



instance Handle GameState where 
    -- the special keys. Exit game, pause game, menu action
    handle (EventKey (SpecialKey KeyEsc)   Down _ _) _               = exitSuccess 
    handle (EventKey (Char 'p') Down _ _) Playing{game = w, state=s} = pure $ Playing w $ pause s
    handle (EventKey (SpecialKey KeyEnter) Down _ _) g@(MainMenu m)  = pure $ menuAction m g 
    -- moving the menu
    handle e (MainMenu m)  = do 
        nm <- handle e m
        return $ MainMenu nm
    -- playing the game
    handle e (Playing w s) = do 
        nw <- handle e w
        return $ Playing nw s
    -- Bossfight WIP
    handle e p = return p

instance Tick GameState where
    -- tick doesn't do anything if the game is paused
    tick f g@Playing{state = Paused _} = pure g
    -- No scrolling screen during bossfight
    tick f (Playing w s@BossFight)            = do 
        nw <- tick f w
        return $ Playing nw s
    -- yes scrolling screen during non boss fights
    tick f (Playing w (Scrolling h))            = do 
        nw <-  tick f $ scroll h w
        return $ Playing nw (Scrolling h)
    -- safety net
    tick _ a                        = pure a 


