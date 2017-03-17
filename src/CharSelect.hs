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

type Screen = Panel Character

initScreen :: Screen
initScreen = character

data Race
  = Human
  | Elf
  | Orc
  deriving (Show, Bounded, Enum)

data Class
  = Warrior
  | Hunter
  | Priest
  | Mage
  deriving (Show, Bounded, Enum)

data Character = Character
  { charName      :: String
  , charRace      :: Race
  , charClass     :: Class
  , charSkinTone  :: Float
  , charAttrs     :: Attrs
  }

data Attrs = Attrs
  { attrStrength  :: Int
  , attrDexterity :: Int
  , attrVitality  :: Int
  , attrEnergy    :: Int
  }

character :: Panel Character
character = Character
  <$> selector "Name" (color white . centeredText) ["John", "Jane"]
  <*> selector_ "Race"
  <*> selector_ "Class"
  <*> skinTone
  <*> attrs

skinTone :: Panel Float
skinTone = fmap g (slider "Skin tone" 0 n white)
  where
    n = 1000
    g i = fromIntegral i / fromIntegral n

attrsTotal :: Attrs -> Int
attrsTotal (Attrs s d v e) = s + d + v + e

attrs :: Panel Attrs
attrs = validatePanel ((<= 10) . attrsTotal) attrsPanel
  where
    attrsPanel = Attrs
      <$> slider "Strength"   0 10 orange
      <*> slider "Dexterity"  0 10 yellow
      <*> slider "Vitality"   0 10 red
      <*> slider "Energy"     0 10 blue

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
