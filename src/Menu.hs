module Menu(Menu, createMenu, menuAction) where

import Classess
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Menu a = Menu { menuItems :: [(String, a)], maxSelected :: Int, selected :: Int } deriving (Read, Show)

instance Paint (Menu a) where
    paint Menu {selected = s, menuItems = items} = 
        let menuSelector = translate 375 (fromIntegral $ 25 +  s * (-200)) $ color yellow $ rectangleWire 800 175
            labels = uncurry (translate 0)  <$> zip [0, (-200)..] (map (scale 0.7 0.7 . color white . text . fst) items)
        in  translate 0 400 $ Pictures (menuSelector : labels)

instance Handle (Menu a) where
    handle e m@Menu{selected = s, maxSelected = ms} =  case e of 
        EventKey (Char 'w') Down _ _ -> m { selected = max 0  $ s-1}
        EventKey (Char 's') Down _ _ -> m { selected = min ms $ s+1}
        _                            -> m

createMenu :: [(String,  a)] -> Menu a

createMenu [] = Menu [] 0 0 
createMenu lst = Menu { menuItems = lst, maxSelected = length lst - 1, selected = 0}

menuAction :: Menu a -> Maybe a
menuAction Menu{menuItems = []} = Nothing
menuAction Menu{menuItems = actions, selected = s} = Just $ map snd actions !! s
