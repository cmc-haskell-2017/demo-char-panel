{-# LANGUAGE DeriveFunctor #-}
module CharSelect.Form where

import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (maybe)
import Data.Monoid

data Field
  = FieldSlider   Slider
  | FieldSelecter Selecter

data Slider = Slider
  { sliderValue     :: Float
  , sliderSelected  :: Bool
  }

data Selecter = Selecter (Maybe Int)

type Fields = [(String, Field)]

data Form a = Form
  { formFields  :: Fields
  , formValue   :: Fields -> Either String a
  } deriving (Functor)

instance Applicative Form where
  pure x = Form [] (const (pure x))

  Form s1 v1 <*> Form s2 v2 = Form s v
    where
      s = s1 <> s2
      v xs = v1 xs <*> v2 xs

translateMouse :: Float -> Float -> Event -> Event
translateMouse dx dy (EventKey k ks m (x, y)) = (EventKey k ks m (x + dx, y + dy))
translateMouse dx dy (EventMotion (x, y)) = EventMotion (x + dx, y + dy)
translateMouse _ _ e = e

mkField :: String -> Field -> (Field -> Either String a) -> Form a
mkField name field value = Form
  { formFields = [(name, field)]
  , formValue  = maybe (Left "no such field") value . lookup name
  }

toSlider :: Field -> Either String Slider
toSlider (FieldSlider s) = Right s
toSlider _ = Left "not a slider"

slider :: String -> Form Float
slider name = mkField name
  (FieldSlider (Slider 0 False))
  (fmap sliderValue . toSlider)

drawForm :: Form a -> Picture
drawForm form = pictures (zipWith drawFieldN [0..] (map snd (formFields form)))

drawFieldN :: Float -> Field -> Picture
drawFieldN n = translate 0 (-fieldHeight * n) . drawField

drawField :: Field -> Picture
drawField (FieldSlider s) = drawSlider s
drawField (FieldSelecter _) = blank

handleForm :: Event -> Form a -> Form a
handleForm e form = form
  { formFields = zipWith (fmap . handleFieldN e) [0..] (formFields form) }

handleFieldN :: Event -> Float -> Field -> Field
handleFieldN e n = handleField (translateMouse 0 (fieldHeight * n) e)

handleField :: Event -> Field -> Field
handleField e (FieldSlider s) = FieldSlider (handleSlider e s)
handleField _ (FieldSelecter s) = FieldSelecter s

drawSlider :: Slider -> Picture
drawSlider (Slider x _) = pictures
  [ color (greyN 0.5) (line [ (-sliderLength/2, 0), (sliderLength/2, 0) ] )
  , color white (translate (sliderLength * (x - 0.5)) 0 (thickCircle (sliderBallRadius/2) sliderBallRadius))
  ]

handleSlider :: Event -> Slider -> Slider
handleSlider (EventKey (MouseButton LeftButton) Down _ mouse) = selectSlider mouse
handleSlider (EventKey (MouseButton LeftButton) Up _ _) = unselectSlider
handleSlider (EventMotion (x, _)) = moveSlider (0.5 + x / sliderLength)
handleSlider _ = id

selectSlider :: Point -> Slider -> Slider
selectSlider (mx, my) (Slider x _)
  | onBall = Slider x True
  where
    onBall = (mx - sliderLength * (x - 0.5))^2 + my^2 <= sliderBallRadius^2
selectSlider _ s = s

unselectSlider :: Slider -> Slider
unselectSlider (Slider x _) = Slider x False

moveSlider :: Float -> Slider -> Slider
moveSlider x (Slider _ True) = Slider (max 0 (min 1 x)) True
moveSlider _ s = s

fieldHeight :: Float
fieldHeight = 100

sliderLength :: Float
sliderLength = 300

sliderBallRadius :: Float
sliderBallRadius = 9

