{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( someFunc
      ,GameState
    ) where

import System.IO (readFile)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import qualified Data.Ord as D (Down(..), comparing) 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Classess as C
import World
import Menu
import System.Random

someFunc :: IO ()
someFunc = do
    seed <- newStdGen
    -- "smart" constructor of menu is used to create the main menu
    let mainMenu = createMenu [("Level 1", loadLevel1 seed), ("HighScore", loadHighScore)]
    playIO FullScreen black 30 MainMenu{ menu = mainMenu} paint handle tick

data WorldState = Paused    { past ::WorldState } 
                | Scrolling {scrollSpeed :: Float} 
                -- | for now there is still no way to get in or out of a bossfight
                | BossFight {} deriving (Show)
pause (Paused s) = s
pause  s         = Paused s

data GameState = Playing    { game :: World, state :: WorldState} 
               | MainMenu   { menu ::Menu GameState} 
               | HighScores { mvps :: IO [(String, Float)]}


--level 1 is wip, ok? OK?
loadLevel1 seed = Playing{ game = startWorld seed, state = Scrolling (-0.5)}

--loading the highscores
loadHighScore = 
    let scores = do 
            file <- readOrCreateFile  "HighScores.txt" 
            let filelines = lines file
            let scores = sortBy (D.comparing $ D.Down . snd) $ map read filelines
            return scores
    in HighScores {mvps = scores}

--crashing the game is ugly so this makes sure that the game survives, even if the highscore file wasn't created
readOrCreateFile :: FilePath -> IO String
readOrCreateFile p = do 
    nothing <- appendFile p ""
    readFile p 

instance Paint GameState where
    --paint menu
    paint MainMenu{ menu = m }         = paint m
    --paint highscores
    paint HighScores{ mvps = ioNames } = do
        scores <- ioNames
        let nameandscores = map (\(name, score) -> Text $ name ++ ":" ++ show score) scores
        let translated = translate 0 <$> [0, (-200)..] <*> nameandscores
        let single = color white $ pictures translated
        return $ translate (-900) 300 single
    --paint paused game
    paint Playing{ game = w, state = Paused s } = do 
        pw <- paint w
        let pauseScreen = translate (-900) (-100) $ scale 4 4 $ color white $ text $ show (Paused s)
        pure $ pictures [pw, pauseScreen]
    --paint game
    paint Playing{ game = w } = paint w



instance Handle GameState where 
    -- the special keys. Exit game, pause game, menu action
    handle (EventKey (SpecialKey KeyEsc)   Down _ _) _               = exitSuccess 
    handle (EventKey (Char 'p') Down _ _) g@Playing{ state = s } = pure $ g{ state = pause s }
    -- if for some the mainmenu failst to activate, it will just stay in the menu
    handle (EventKey (SpecialKey KeyEnter) Down _ _) mm@MainMenu{ menu = m }  = pure $ fromMaybe mm $ menuAction m 
    -- moving the menu
    handle e mm@MainMenu{ menu = m }  = do 
        nm <- handle e m
        return $ mm{ menu = nm}
    -- playing the game
    handle e g@Playing{ game = w } = do 
        nw <- handle e w
        return $ g{ game = nw }
    -- Bossfight WIP
    handle e p = return p

instance Tick GameState where
    -- tick doesn't do anything if the game is paused
    tick f g@Playing{state = Paused _} = pure g
    -- No scrolling screen during bossfight
    tick f g@Playing{ game = w, state = BossFight}            = do 
        nw <- tick f w
        return $ g{ game = nw }
    -- yes scrolling screen during non boss fights
    tick f g@Playing{ game = w, state = Scrolling h }            = do 
        nw <-  tick f $ scroll h w
        return $ g{ game = nw }
    -- safety net
    tick _ a                        = pure a 


