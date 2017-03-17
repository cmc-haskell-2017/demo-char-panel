{-# LANGUAGE DeriveFunctor #-}
module CharSelect.Panel (
  Panel,
  drawPanel,
  handlePanel,
  validatePanel,

  slider,
  selector,
  selector_,

  centeredText,
) where

import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (maybe)
import Data.Monoid

data Field
  = FieldSlider   Slider
  | FieldSelector Selector

-- | Слайдер.
data Slider = Slider
  { sliderMin       :: Int    -- ^ Минимальное значение слайдера.
  , sliderMax       :: Int    -- ^ Максимальное значение слайдера.
  , sliderValue     :: Int    -- ^ Текущее значение слайдера.
  , sliderSelected  :: Bool   -- ^ Выбран ли слайдер?
  , sliderColor     :: Color  -- ^ Цвет слайдера.
  }

-- | Элемент выбора.
data Selector = Selector
  { selectorPictures  :: [Picture]  -- ^ Изображения различных вариантов.
  , selectorIndex     :: Int        -- ^ Номер выбранного варианта.
  }

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

mkField :: String -> Field -> (Field -> Maybe a) -> Panel a
mkField name field value = Panel
  { panelFields = [(name, field)]
  , panelValue  = maybe Nothing value . lookup name
  }

toSlider :: Field -> Maybe Slider
toSlider (FieldSlider s) = Just s
toSlider _ = Nothing

initSlider :: Int -> Int -> Color -> Slider
initSlider minValue maxValue c = Slider
  { sliderMin       = minValue
  , sliderMax       = maxValue
  , sliderValue     = minValue
  , sliderSelected  = False
  , sliderColor     = c
  }

slider :: String -> Int -> Int -> Color -> Panel Int
slider name minValue maxValue c = mkField name
  (FieldSlider (initSlider minValue maxValue c))
  (fmap sliderValue . toSlider)

toSelector :: Field -> Maybe Selector
toSelector (FieldSelector s) = Just s
toSelector _ = Nothing

initSelector :: [Picture] -> Selector
initSelector ps = Selector
  { selectorPictures = ps
  , selectorIndex    = 0
  }

selector :: String -> (a -> Picture) -> [a] -> Panel a
selector name drawValue values = mkField name
  (FieldSelector (initSelector (map drawValue values)))
  (fmap (\s -> values !! selectorIndex s) . toSelector)

selector_ :: (Show a, Bounded a, Enum a) => String -> Panel a
selector_ name = selector name (color white . centeredText . show) [minBound..maxBound]

drawPanel :: Panel a -> Picture
drawPanel panel = pictures (zipWith drawFieldN [0..] (map snd (panelFields panel)))

drawFieldN :: Float -> Field -> Picture
drawFieldN n = translate 0 (-fieldHeight * n) . drawField

drawField :: Field -> Picture
drawField (FieldSlider s) = drawSlider s
drawField (FieldSelector s) = drawSelector s

handlePanel :: Event -> Panel a -> Panel a
handlePanel e panel = case panelValue panel newFields of
  Nothing -> panel
  Just _  -> panel { panelFields = newFields }
  where
    newFields = zipWith (fmap . handleFieldN e) [0..] (panelFields panel)

validatePanel :: (a -> Bool) -> Panel a -> Panel a
validatePanel p panel = panel { panelValue = newValue }
  where
    newValue fs = case panelValue panel fs of
      Just x | p x -> Just x
      _ -> Nothing

handleFieldN :: Event -> Float -> Field -> Field
handleFieldN e n = handleField (translateMouse 0 (fieldHeight * n) e)

handleField :: Event -> Field -> Field
handleField e (FieldSlider s) = FieldSlider (handleSlider e s)
handleField e (FieldSelector s) = FieldSelector (handleSelector e s)

drawSlider :: Slider -> Picture
drawSlider s = pictures
  [ color (greyN 0.5) (line [ (-w/2, 0), (w/2, 0) ] )
  , color (sliderColor s) (polygon [ (-w/2, -r/4), (dx, -r/4), (dx, r/4), (-w/2, r/4) ])
  , color white (translate dx 0 (thickCircle (r/2) r))
  ]
  where
    dx = w * (sliderPosition s - 0.5)
    w = sliderLength
    r = sliderBallRadius

sliderPosition :: Slider -> Float
sliderPosition s = x / w
  where
    x = fromIntegral (sliderValue s - sliderMin s)
    w = fromIntegral (sliderMax s - sliderMin s)

drawSelector :: Selector -> Picture
drawSelector s = pictures
    [ translate (-dx) 0 (rotate 180 arrow)
    , selected
    , translate dx 0 arrow ]
  where
    selected = selectorPictures s !! selectorIndex s
    arrow = color white (pictures
      [ polygon [ (w/5, 0), (w, 0), (0,  w/2) ]
      , polygon [ (w/5, 0), (w, 0), (0, -w/2) ]
      ])
    w = selectorArrowWidth
    dx = selectorWidth / 2 - w

-- | Центрированный текст, отмасштабированный таким образом,
-- чтобы хорошо смотреться в элементе выбора.
centeredText :: String -> Picture
centeredText s = translate dx dy (scale z z (text s))
  where
    z = selectorWidth * 0.001
    w = 80 * z
    dx = - w * fromIntegral (length s) / 2
    dy = - w / 2

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

handleSelector :: Event -> Selector -> Selector
handleSelector (EventKey (MouseButton LeftButton) Down _ mouse) = handleSelectorClick mouse
handleSelector _ = id

handleSelectorClick :: Point -> Selector -> Selector
handleSelectorClick (mx, my) s
  | onLeftArrow  = selectPrevious s
  | onRightArrow = selectNext s
  | otherwise    = s
  where
    w  = selectorArrowWidth
    sw = selectorWidth
    onLeftArrow = and
      [ -w/2 <= my && my <= w/2
      , -sw/2 <= mx && mx <= -sw/2 + w ]
    onRightArrow = and
      [ -w/2 <= my && my <= w/2
      , sw/2 - w <= mx && mx <= sw/2]

selectPrevious :: Selector -> Selector
selectPrevious s = s { selectorIndex = (selectorIndex s - 1) `mod` length (selectorPictures s) }

selectNext :: Selector -> Selector
selectNext s = s { selectorIndex = (selectorIndex s + 1) `mod` length (selectorPictures s) }

fieldHeight :: Float
fieldHeight = 50

sliderLength :: Float
sliderLength = fieldHeight * 5

sliderBallRadius :: Float
sliderBallRadius = 9

selectorWidth :: Float
selectorWidth = sliderLength

selectorArrowWidth :: Float
selectorArrowWidth = selectorWidth / 10
