module CharSelect.Panel.Field where

import Graphics.Gloss.Interface.Pure.Game

import CharSelect.Utils

-- | Поля ввода.
data Field
  = FieldSlider   Slider    -- ^ Слайдер.
  | FieldSelector Selector  -- ^ Элемент выбора.

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

-- | Именованное поле.
type NamedField = (String, Field)

-- | Список именованных полей.
type Fields = [NamedField]

-- | Попытаться привести поле к слайдеру.
toSlider :: Field -> Maybe Slider
toSlider (FieldSlider s) = Just s
toSlider _ = Nothing

-- | Попытаться привести поле к элементу выбора.
toSelector :: Field -> Maybe Selector
toSelector (FieldSelector s) = Just s
toSelector _ = Nothing

-- | Инициализировать слайдер.
initSlider
  :: Int    -- ^ Минимальное значение.
  -> Int    -- ^ Максимальное значение.
  -> Color  -- ^ Цвет слайдера.
  -> Slider
initSlider minValue maxValue c = Slider
  { sliderMin       = minValue
  , sliderMax       = maxValue
  , sliderValue     = minValue
  , sliderSelected  = False
  , sliderColor     = c
  }

-- | Инициализировать элемент выбора.
initSelector :: [Picture] -> Selector
initSelector ps = Selector
  { selectorPictures = ps
  , selectorIndex    = 0
  }

-- | Положение слайдера (от 0 до 1).
sliderPosition :: Slider -> Float
sliderPosition s = x / w
  where
    x = fromIntegral (sliderValue s - sliderMin s)
    w = fromIntegral (sliderMax s - sliderMin s)

-- | Текущий выбранный объект.
selectorCurrent :: Selector -> Picture
selectorCurrent s = selectorPictures s !! selectorIndex s

-- | Отобразить именованное поле.
drawNamedField
  :: (String -> Picture)  -- ^ Функция отображения имени поля.
  -> NamedField           -- ^ Именованное поле.
  -> Picture
drawNamedField drawFieldName (name, field) = pictures
  [ translate (- fieldWidth) 0 (drawFieldName name)
  , drawField field
  ]

-- | Отобразить поле.
drawField :: Field -> Picture
drawField (FieldSlider   s) = drawSlider s
drawField (FieldSelector s) = drawSelector s

-- | Отобразить слайдер.
drawSlider :: Slider -> Picture
drawSlider s = pictures
  [ color (withAlpha 0.5 black) sliderLine
  , color (sliderColor s) sliderThickLine
  , sliderBall
  ]
  where
    sliderBall = translate dx 0 (thickCircle (r/2) r)
    sliderLine = line [ (-w/2, 0), (w/2, 0) ]
    sliderThickLine = rect (-w/2, -r/4) (dx, r/4)

    dx = w * (sliderPosition s - 0.5)
    w = sliderLength
    r = sliderBallRadius

-- | Отобразить элемент выбора.
drawSelector :: Selector -> Picture
drawSelector s = pictures
    [ translate (-dx) 0 (rotate 180 arrow)
    , selectorCurrent s
    , translate dx 0 arrow ]
  where
    arrow = pictures
      [ polygon [ (w/5, 0), (w, 0), (0,  w/2) ]
      , polygon [ (w/5, 0), (w, 0), (0, -w/2) ]
      ]
    w = selectorArrowWidth
    dx = selectorWidth / 2 - w

-- | Обработать события поля.
handleField :: Event -> Field -> Field
handleField e (FieldSlider s) = FieldSlider (handleSlider e s)
handleField e (FieldSelector s) = FieldSelector (handleSelector e s)

-- | Обработать события слайдера.
handleSlider :: Event -> Slider -> Slider
handleSlider e = case e of
  EventKey (MouseButton LeftButton) Down _ mouse -> selectSlider mouse
  EventKey (MouseButton LeftButton) Up _ _ -> unselectSlider
  EventMotion (x, _) -> moveSlider (0.5 + x / sliderLength)
  _ -> id

-- | Выделить шарик слайдера.
selectSlider :: Point -> Slider -> Slider
selectSlider (mx, my) s
  | onBall    = s { sliderSelected = True }
  | otherwise = s
  where
    onBall = (mx - sliderLength * (x - 0.5))^2 + my^2 <= sliderBallRadius^2
    x = sliderPosition s

-- | Убрать выделения с шарика слайдера.
unselectSlider :: Slider -> Slider
unselectSlider s = s { sliderSelected = False }

-- | Передвинуть шарик слайдера.
moveSlider :: Float -> Slider -> Slider
moveSlider x s
  | sliderSelected s = s { sliderValue = newSliderValue }
  | otherwise = s
  where
    i = sliderMin s + round (x * fromIntegral (sliderMax s - sliderMin s))
    newSliderValue = max (sliderMin s) (min (sliderMax s) i)

-- | Обработать события элемента выбора.
handleSelector :: Event -> Selector -> Selector
handleSelector (EventKey (MouseButton LeftButton) Down _ mouse) = handleSelectorClick mouse
handleSelector _ = id

-- | Обработать клик мыши.
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

-- | Выбрать предыдущий вариант.
selectPrevious :: Selector -> Selector
selectPrevious s = s { selectorIndex = (selectorIndex s - 1) `mod` length (selectorPictures s) }

-- | Выбрать следующий вариант.
selectNext :: Selector -> Selector
selectNext s = s { selectorIndex = (selectorIndex s + 1) `mod` length (selectorPictures s) }

-- | Высота одного поля.
fieldHeight :: Float
fieldHeight = 60

-- | Ширина одного поля.
fieldWidth :: Float
fieldWidth = 5 * fieldHeight

-- | Длина слайдера.
sliderLength :: Float
sliderLength = fieldWidth

-- | Радиус шарика слайдера.
sliderBallRadius :: Float
sliderBallRadius = fieldHeight / 6

-- | Ширина элемента выбора.
selectorWidth :: Float
selectorWidth = fieldWidth

-- | Ширина одной из стрелок элемента выбора.
selectorArrowWidth :: Float
selectorArrowWidth = selectorWidth / 10
