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

data Screen = Screen
  { screenPanel :: Panel Character
  , screenChar  :: Maybe Character
  }

updateScreenPanel :: (Panel Character -> Panel Character) -> Screen -> Screen
updateScreenPanel f screen = screen
  { screenPanel = newPanel
  , screenChar  = readPanel newPanel
  }
  where
    newPanel = f (screenPanel screen)

initScreen :: Screen
initScreen = Screen
  { screenPanel = character
  , screenChar  = readPanel character
  }

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
drawScreen screen = pictures
  [ uncurry translate panelOffset (drawPanel (screenPanel screen))
  , uncurry translate charOffset (foldMap drawCharacter (screenChar screen))
  ]

drawCharacter :: Character -> Picture
drawCharacter c = scale charSize charSize (pictures
  [ color (charSkinColor c) drawBody
  , drawClassClothes (charClass c)
  ])

-- | Тело человека с головой.
drawBody :: Picture
drawBody = pictures
  -- голова
  [ translate 0 11 (thickCircle 4.5 9)
  -- туловище
  , polygon [ (-11, 0), (-11, -42), (11, -42), (11, 0) ]
  -- левое плечо
  , polygon [ (-11, -10), (-11, -15), (-21, -15), (-21, -10) ]
  , translate (-11) (-10) (thickArc 90 180 5 10)
  -- левая рука
  , polygon [ (-21, -15), (-21, -39), (-13, -39), (-13, -15) ]
  , translate (-17) (-39) (thickCircle 2 4)
  -- правое плечо
  , polygon [ (11, -10), (11, -15), (21, -15), (21, -10) ]
  , translate 11 (-10) (thickArc 0 90 5 10)
  -- правая рука
  , polygon [ (21, -15), (21, -39), (13, -39), (13, -15) ]
  , translate 17 (-39) (thickCircle 2 4)
  -- левая нога
  , polygon [ (-11, -42), (-11, -82), (-1, -82), (-1, -42) ]
  , translate (-6) (-82) (thickCircle 2.5 5)
  -- правая нога
  , polygon [ (11, -42), (11, -82), (1, -82), (1, -42) ]
  , translate 6 (-82) (thickCircle 2.5 5)
  ]

drawClassClothes :: Class -> Picture
drawClassClothes Warrior = drawWarriorArmor
drawClassClothes _ = blank

drawWarriorArmor :: Picture
drawWarriorArmor = pictures
  [ color (dark orange) pants
  , color (greyN 0.5) armor
  ]
  where
    armor = pictures
      -- броня
      [ polygon [ (-11.5, 0), (-11.5, -42.5), (11.5, -42.5), (11.5, 0) ]
      -- левый наплечник
      , polygon [ (-11, -10), (-11, -15), (-23, -15), (-23, -10) ]
      , translate (-11) (-10) (thickArc 90 180 6 12)
      -- правый наплечник
      , polygon [ (11, -10), (11, -15), (23, -15), (23, -10) ]
      , translate 11 (-10) (thickArc 0 90 6 12)
      ]
    pants = pictures
      [ polygon [ (-11.5, -41), (-11.5, -45), (11.5, -45), (11.5, -41) ]
      -- левая нога
      , polygon [ (-11.5, -42), (-11.5, -72), (-0.5, -72), (-0.5, -42) ]
      -- правая нога
      , polygon [ (11.5, -42), (11.5, -72), (0.5, -72), (0.5, -42) ]
      ]

charSkinColor :: Character -> Color
charSkinColor c = mixColors (1 - t) t darkSkinColor lightSkinColor
  where
    t = charSkinTone c
    (darkSkinColor, lightSkinColor) = raceSkinColorRange (charRace c)

raceSkinColorRange :: Race -> (Color, Color)
raceSkinColorRange Human = (makeColorI 141 85 36 255, makeColorI 255 219 172 255)
raceSkinColorRange Elf   = (makeColorI 85 36 141 255, makeColorI 219 172 255 255)
raceSkinColorRange Orc   = (makeColorI 85 141 36 255, makeColorI 219 255 172 255)

handleScreen :: Event -> Screen -> Screen
handleScreen = updateScreenPanel . handlePanel . uncurry translateMouse (- panelOffset)

updateScreen :: Float -> Screen -> Screen
updateScreen _ = id

screenWidth :: Num a => a
screenWidth = 1200

screenHeight :: Num a => a
screenHeight = 675

charSize :: Float
charSize = 3

panelOffset :: (Float, Float)
panelOffset = (-200, 150)

charOffset :: (Float, Float)
charOffset = (150, 100)

