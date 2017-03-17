module CharSelect where

import Graphics.Gloss.Interface.Pure.Game

import CharSelect.Panel

charSelectScreen :: IO ()
charSelectScreen =
  play display bgColor fps initScreen drawScreen handleScreen updateScreen
  where
    display = InWindow "Экран выбора персонажа" (screenWidth, screenHeight) (100, 100)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

type Screen = Panel Int

initScreen :: Screen
initScreen = slider "ololo" 0 4 <* slider "alala" 0 23 <* slider "elele" 0 19

drawScreen :: Screen -> Picture
drawScreen = drawPanel

handleScreen :: Event -> Screen -> Screen
handleScreen = handlePanel

updateScreen :: Float -> Screen -> Screen
updateScreen _ = id

screenWidth :: Num a => a
screenWidth = 1200

screenHeight :: Num a => a
screenHeight = 675
