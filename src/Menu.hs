module Menu(Menu, createMenu, menuAction) where

import Classess
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Menu a = Menu { menuItems :: [(String, a)], maxSelected :: Int, selected :: Int }

instance Paint (Menu a) where
    paint Menu {selected = s, menuItems = items} = 
        let menuSelector = translate 400 (fromIntegral $ 50 + s * (-250)) $ color yellow $ rectangleWire 900 200
            labels = uncurry (translate 0) <$> zip [0, (-250)..] (map (color white . text . fst) items)
        in  pure $ translate (-50) 250 $ Pictures (menuSelector : labels)

instance Handle (Menu a) where
    handle e m@Menu{selected = s, maxSelected = ms} = pure $ case e of 
        EventKey (Char 'w') Down _ _ -> m { selected = max 0  $ s-1}
        EventKey (Char 's') Down _ _ -> m { selected = min ms $ s+1}
        _                            -> m

createMenu :: [(String,  a)] -> Menu a

createMenu [] = Menu [] 0 0 
createMenu lst = Menu { menuItems = lst, maxSelected = length lst - 1, selected = 0}

menuAction :: Menu a -> Maybe a
menuAction Menu{menuItems = []} = Nothing
menuAction Menu{menuItems = actions, selected = s} = Just $ map snd actions !! s
