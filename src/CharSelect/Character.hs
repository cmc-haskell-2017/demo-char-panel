module CharSelect.Character where

-- | Пол персонажа.
data Sex
  = Male    -- ^ Мальчик.
  | Female  -- ^ Девочка.
  deriving (Eq, Show, Bounded, Enum)

-- | Раса персонажа.
data Race
  = Human   -- ^ Человек.
  | Elf     -- ^ Эльф.
  | Orc     -- ^ Орк.
  deriving (Eq, Show, Bounded, Enum)

-- | Класс персонажа.
data Class
  = NoClass   -- ^ Без класса.
  | Warrior   -- ^ Воин.
  | Mage      -- ^ Маг.
  deriving (Eq, Show, Bounded, Enum)

-- | Персонаж.
data Character = Character
  { charType      :: CharType -- ^ Раса, класс.
  , charSkinTone  :: Float    -- ^ Оттенок кожи (темнее/светлее).
  , charAttrs     :: Attrs    -- ^ Атрибуты.
  }

-- | Тип персонажа.
-- Определяется расой и классом.
data CharType = CharType
  { charSex     :: Sex      -- ^ Пол.
  , charRace    :: Race     -- ^ Раса.
  , charClass   :: Class    -- ^ Класс.
  } deriving (Eq)

-- | Атрибуты персонажа, влияющие на способности.
data Attrs = Attrs
  { attrStrength  :: Int  -- ^ Сила.
  , attrDexterity :: Int  -- ^ Ловкость.
  , attrVitality  :: Int  -- ^ Здоровье.
  , attrEnergy    :: Int  -- ^ Энергия.
  }

allSexes :: [Sex]
allSexes = [minBound..maxBound]

allRaces :: [Race]
allRaces = [minBound..maxBound]

allClasses :: [Class]
allClasses = [minBound..maxBound]

allCharTypes :: [CharType]
allCharTypes = CharType
  <$> allSexes
  <*> allRaces
  <*> allClasses
