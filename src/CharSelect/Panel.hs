{-# LANGUAGE DeriveFunctor #-}
module CharSelect.Panel where

import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (maybe)
import Data.Monoid

data Field
  = FieldSlider   Slider
  | FieldSelecter Selecter

-- | Слайдер.
data Slider = Slider
  { sliderMin       :: Int    -- ^ Минимальное значение слайдера.
  , sliderMax       :: Int    -- ^ Максимальное значение слайдера.
  , sliderValue     :: Int    -- ^ Текущее значение слайдера.
  , sliderSelected  :: Bool   -- ^ Выбран ли слайдер?
  }

data Selecter = Selecter (Maybe Int)

type Fields = [(String, Field)]

data Panel a = Panel
  { panelFields  :: Fields
  , panelValue   :: Fields -> Maybe a
  } deriving (Functor)

instance Applicative Panel where
  pure x = Panel [] (pure (pure x))

  Panel f1 v1 <*> Panel f2 v2 = Panel f v
    where
      f = f1 <> f2
      v fs = v1 fs <*> v2 fs

translateMouse :: Float -> Float -> Event -> Event
translateMouse dx dy (EventKey k ks m (x, y)) = (EventKey k ks m (x + dx, y + dy))
translateMouse dx dy (EventMotion (x, y)) = EventMotion (x + dx, y + dy)
translateMouse _ _ e = e

updateField :: String -> (Field -> Field) -> Fields -> Fields
updateField name g ((k, v) : fs)
  | k == name = (name, g v) : fs
updateField _ _ fs = fs

mkField :: String -> Field -> (Field -> Maybe a) -> Panel a
mkField name field value = Panel
  { panelFields = [(name, field)]
  , panelValue  = maybe Nothing value . lookup name
  }

toSlider :: Field -> Maybe Slider
toSlider (FieldSlider s) = Just s
toSlider _ = Nothing

withSlider :: (Slider -> Slider) -> Field -> Field
withSlider g (FieldSlider s) = FieldSlider (g s)
withSlider _ f = f

initSlider :: Int -> Int -> Slider
initSlider minValue maxValue = Slider
  { sliderMin       = minValue
  , sliderMax       = maxValue
  , sliderValue     = minValue
  , sliderSelected  = False
  }

slider :: String -> Int -> Int -> Panel Int
slider name minValue maxValue = mkField name
  (FieldSlider (initSlider minValue maxValue))
  (fmap sliderValue . toSlider)

drawPanel :: Panel a -> Picture
drawPanel panel = pictures (zipWith drawFieldN [0..] (map snd (panelFields panel)))

drawFieldN :: Float -> Field -> Picture
drawFieldN n = translate 0 (-fieldHeight * n) . drawField

drawField :: Field -> Picture
drawField (FieldSlider s) = drawSlider s
drawField (FieldSelecter _) = blank

handlePanel :: Event -> Panel a -> Panel a
handlePanel e panel = panel
  { panelFields = zipWith (fmap . handleFieldN e) [0..] (panelFields panel) }

handleFieldN :: Event -> Float -> Field -> Field
handleFieldN e n = handleField (translateMouse 0 (fieldHeight * n) e)

handleField :: Event -> Field -> Field
handleField e (FieldSlider s) = FieldSlider (handleSlider e s)
handleField _ (FieldSelecter s) = FieldSelecter s

drawSlider :: Slider -> Picture
drawSlider s = pictures
  [ color (greyN 0.5) (line [ (-sliderLength/2, 0), (sliderLength/2, 0) ] )
  , color white (translate (sliderLength * (x - 0.5)) 0 (thickCircle (sliderBallRadius/2) sliderBallRadius))
  ]
  where
    x = sliderPosition s

sliderPosition :: Slider -> Float
sliderPosition s = x / w
  where
    x = fromIntegral (sliderValue s - sliderMin s)
    w = fromIntegral (sliderMax s - sliderMin s)

handleSlider :: Event -> Slider -> Slider
handleSlider (EventKey (MouseButton LeftButton) Down _ mouse) = selectSlider mouse
handleSlider (EventKey (MouseButton LeftButton) Up _ _) = unselectSlider
handleSlider (EventMotion (x, _)) = moveSlider (0.5 + x / sliderLength)
handleSlider _ = id

selectSlider :: Point -> Slider -> Slider
selectSlider (mx, my) s
  | onBall    = s { sliderSelected = True }
  | otherwise = s
  where
    onBall = (mx - sliderLength * (x - 0.5))^2 + my^2 <= sliderBallRadius^2
    x = sliderPosition s

unselectSlider :: Slider -> Slider
unselectSlider s = s { sliderSelected = False }

moveSlider :: Float -> Slider -> Slider
moveSlider x s
  | sliderSelected s = s { sliderValue = newSliderValue }
  | otherwise = s
  where
    i = sliderMin s + round (x * fromIntegral (sliderMax s - sliderMin s))
    newSliderValue = max (sliderMin s) (min (sliderMax s) i)

fieldHeight :: Float
fieldHeight = 100

sliderLength :: Float
sliderLength = 300

sliderBallRadius :: Float
sliderBallRadius = 9

