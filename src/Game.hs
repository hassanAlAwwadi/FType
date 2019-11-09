module Game where

import System.Exit(exitSuccess)
import System.IO (readFile) 
import Data.Maybe as M(fromMaybe)
import Data.List as S(sortBy) 
import qualified Data.Ord as D (Down(..), comparing) 

import Classess as C
import World(World, lives, score, WorldState(..), pause, scroll)
import Menu(Menu, createMenu, menuAction)

import Graphics.Gloss.Interface.IO.Game

data Game   = Playing    { world :: World, state :: WorldState} 
            | MainMenu   { menu  :: Menu Game } 
            -- | Highcores are really the only reason we need to use playIO instead of regular play
            | HighScores { mvps  :: IO [(String, Float)]}


instance Creatable Game where
    create stat dyn = MainMenu $ createMenu [("play game", Playing w $ Scrolling (-0.5)), ("HighScore", loadHighScore (readOrCreateFile  "HighScores.txt") )] where
        w = create stat dyn


--loading the highscores
loadHighScore :: IO String -> Game
loadHighScore file= 
    let scores = do 
            file' <- file
            let filelines = lines file'
            pure $ sortBy (D.comparing $ D.Down . snd) $ map read filelines
    in HighScores { mvps = scores }

--write the highscores
writeHighScore :: Int -> IO String
writeHighScore score = do 
            file <- readOrCreateFile  "HighScores.txt" 
            length file `seq` writeFile "HighScores.txt" ("(\"Player1\", "++ show score ++")\n")
            appendFile "HighScores.txt" file
            return file
        
--crashing the game is ugly so this makes sure that the game survives, even if the highscore file wasn't created
readOrCreateFile :: FilePath -> IO String
readOrCreateFile p = do 
    appendFile p ""
    readFile p 

instance PaintIO Game where
    --paint menu
    paintIO MainMenu{ menu = m }         = pure $ paint m
    --paint highscores
    paintIO HighScores{ mvps = ioNames } = do
        scores <- ioNames
        let nameandscores = map (\(name, score) -> Text $ name ++ ":" ++ show score) scores
        let translated = uncurry (translate 0) <$> zip [0, (-200)..] nameandscores
        let single = color white $ pictures translated
        return $ translate (-900) 300 single
    --paint paused game
    paintIO Playing{ world = w, state = Paused s } = do 
        let pw = paint w
        let pauseScreen = translate (-900) (-100) $ scale 4 4 $ color white $ text $ show (Paused s)
        pure $ pictures [pw, pauseScreen]
    --paint game
    paintIO Playing{ world = w } = pure $ paint w



instance HandleIO Game where 
    -- the special keys. Exit game, pause game, menu action
    handleIO (EventKey (SpecialKey KeyEsc)   Down _ _) _               = exitSuccess 
    handleIO (EventKey (Char 'p') Down _ _) g@Playing{ state = s } = pure $ g{ state = pause s }
    -- if for some the mainmenu failst to activate, it will just stay in the menu
    handleIO (EventKey (SpecialKey KeyEnter) Down _ _) mm@MainMenu{ menu = m }  = pure $ fromMaybe mm $ menuAction m 
    -- moving the menu
    handleIO e mm@MainMenu{ menu = m }  = return $ mm{ menu = handle e m}
    -- playing the game
    handleIO e g@Playing{ world = w } = return $ g{ world = handle e w }
    -- Bossfight WIP
    handleIO _ p = pure p

instance TickIO Game where
    -- tick doesn't do anything if the game is paused
    tickIO _ g@Playing{state = Paused _} = pure g
    -- No scrolling screen during bossfight
    tickIO f g@Playing{ world = w, state = BossFight _} = return $ g{ world = tick f w }
    -- yes scrolling screen during non boss fights
    tickIO f g@Playing{ world = w, state = Scrolling h } | lives w == 0 &&score w>0 =  tickIO 0 (loadHighScore (writeHighScore (score w)))
                                                         | otherwise = return $ g{ world = tick f $ scroll h w }
    -- menu/highscore tick doesn't do anything
    tickIO _ g = pure g  
