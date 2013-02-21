module HashPoke
  ( Type(..)
  , Stats(..)
  , Evolution(..)
  , Species(..)
  , Move(..)
  , Monster(..)
  , MonsterMove(..)
  , Trainer(..)
  , maxHP
  , stat
  , damage
  ) where

import Data.List

-- Posibles tipos para un Monster o Move
data Type
  = Bug
  | Dark
  | Dragon
  | Electric
  | Fighting
  | Fire
  | Flying
  | Ghost
  | Grass
  | Ground
  | Ice
  | Normal
  | Poison
  | Psychic
  | Rock
  | Steel
  | Water
  deriving (Bounded, Eq, Enum, Read, Show)

-- Estadisticas de un Pokemon
data Stats = 
  Stats 
    { hp        :: Int
    , attack    :: Int
    , defense   :: Int
    , spAttack  :: Int
    , spDefense :: Int
    , speed     :: Int
    } deriving (Eq, Read, Show)

-- Evolucion de un Pokemon a otro Pokemon
data Evolution = 
  Evolution
    { eNo      :: Int      -- Indicador de numero en Pokedex
    , criterio :: String   -- Criterio de evolucion
    } deriving (Eq, Read, Show)

-- Datos relevantes para una especie de un Pokemon
data Species = 
  Species
    { no           :: Int
    , name         :: String
    , pokeType     :: [Type]
    , base         :: Stats
    , preEvolution :: Maybe Evolution
    , evolutions   :: [Evolution]
    } deriving (Eq, Read)

instance Show (Species) where
  show (Species no name pokeType base preEvolution evolutions) = 
    "{" ++ show no ++ ", " ++ show name ++ ", "
    ++ show pokeType ++ ", Base " ++ show base ++ ", Evolutions "
    ++ show (map eNo evolutions) ++ "}\n"

-- Representa un ataque
data Move = 
  Move
    { moveName :: String
    , moveType :: Type
    , physical :: Bool
    , pp       :: Int
    , power    :: Int
    } deriving (Eq, Read)

instance Show (Move) where
  show (Move moveName moveType physical pp power) = 
    "{" ++ show moveName ++ ", " ++ show moveType 
    ++ (if physical then ", Physical" else "") ++
    ", " ++ show pp ++ "PP, PWR " ++ show power ++ "}\n"

-- Para asociar un ataque con un monstruo, con los PP que le quedan disponibles
data MonsterMove = 
  MonsterMove 
    { monMove :: Move   -- Move del Monster
    , monPP   :: Int    -- PP restantes para este Move de este Monster
    } deriving (Eq, Read, Show)

-- Representa un monstruo especifico (como ser una instancia de la clase Species?)
data Monster = 
  Monster 
    { species  :: Species
    , nickname :: String
    , lvl      :: Int
    , presHP   :: Int
    , moves    :: [MonsterMove]
    , stats    :: Stats
    , iv       :: Stats
    , ev       :: Stats
    } deriving (Eq, Read)

instance Show (Monster) where
  show (Monster species nickname lvl presHP moves stats iv ev) = 
    "{(" ++ show (no species) ++ ", " ++ show (name species)
    ++ ")\nNickname: " ++ show nickname ++ "\nLVL: " ++ show lvl
    ++ "\nStats\n  HP:        " ++ show presHP ++ "/" ++ show (hp stats)
    ++ "\n  Attack:    " ++ show (attack stats) ++ "\n  Defense:   "
    ++ show (defense stats) ++ "\n  SpAttack:  " ++ show (spAttack stats)
    ++ "\n  SpDefense: " ++ show (spDefense stats) ++ "\n  Speed:     "
    ++ show (speed stats) ++ "\nMoves\n" ++ show (map monMove moves) ++ "}\n"

-- Lista de Pokemones de un entrenador y un indicador de cual esta en uso ahora
data Trainer = 
  Trainer 
    { active     :: Int         --Entero que indica cual posicion de la lista de Pokemones es el actual
    , pokeballs  :: [Monster]   --Lista de Pokemones de un entrenador
    } deriving (Eq, Read)

instance Show (Trainer) where 
  show (Trainer active pokeballs) = 
    "{ Battling Pokemon: " ++ (show $ nickname $ pokeballs !! active) ++ " HP " 
    ++ (show $ presHP $ pokeballs !! active) ++ "/" ++ (show $ hp $ stats $ pokeballs !! active)
    ++ " LVL" ++ (show $ lvl $ pokeballs !! active) ++ "\n  PokeParty: |" 
    ++ foldr (\x -> (++) ((show $ nickname x) ++ "|")) "" pokeballs ++ " }"

-- Determina, para un Type de ataque, cuales tipos son super efectivos,
-- cuales tipos son resistentes y cuales son inmunes.
moveTypeRelation :: Type      -- Type de ataque a determinar la relación.
                 -> ( [Type]  -- Type al que es super efectivo (2x dano). (| Type es super efectivo a [Type])
                    , [Type]  -- Type que lo resisten (0.5x dano).        (| Type ataca con resistencia a [Type])
                    , [Type]  -- Type inmunes a el (0x dano).             (| Type no le hace dano a Pokemones de [Type])
                    )
moveTypeRelation x
  | Bug      <- x = ([Grass, Psychic, Dark], [Fighting, Flying, Poison, Ghost, Steel, Fire], [])
  | Dark     <- x = ([Ghost, Psychic], [Fighting, Steel, Dark], [])
  | Dragon   <- x = ([Dragon], [Steel], [])
  | Electric <- x = ([Flying, Water], [Grass, Electric, Dragon], [Ground])
  | Fighting <- x = ([Normal, Rock, Steel, Ice, Dark], [Flying, Poison, Bug, Psychic], [Ghost])
  | Fire     <- x = ([Bug, Steel, Grass, Ice], [Rock, Fire, Water, Dragon], [])
  | Flying   <- x = ([Fighting, Bug, Grass], [Rock, Steel, Electric], [])
  | Ghost    <- x = ([Ghost, Psychic], [Steel, Dark], [Normal])
  | Grass    <- x = ([Ground, Rock, Water], [Flying, Poison, Bug, Steel, Fire, Grass, Dragon], [])
  | Ground   <- x = ([Poison, Rock, Steel, Fire, Electric], [Bug, Grass], [Flying])
  | Ice      <- x = ([Flying, Ground, Grass, Dragon], [Steel, Fire, Water], [])
  | Normal   <- x = ([], [Rock, Steel], [Ghost])
  | Poison   <- x = ([Grass], [Poison, Ground, Rock, Ghost], [Steel])
  | Psychic  <- x = ([Fighting, Poison], [Steel, Psychic], [Dark])
  | Rock     <- x = ([Flying, Bug, Fire, Ice], [Fighting, Ground, Steel], [])
  | Steel    <- x = ([Rock, Ice], [Steel, Fire, Water, Electric], [])
  | Water    <- x = ([Ground, Rock, Fire], [Water, Grass, Dragon], [])

-- Determina para un Monster en especifico su HP maximo
maxHP :: Monster -> Int
maxHP mon = floor $ 10 + ((*) mLVL $ mIV + 2 * mBase + mEV / 4 + 100) / 100
  where
    mLVL  = fromIntegral.lvl $ mon
    mIV   = fromIntegral.hp.iv $ mon
    mEV   = fromIntegral.hp.ev $ mon
    mBase = fromIntegral.hp.base.species $ mon

-- Determina para un Monster en especifico cierta estadistica
stat :: Monster         -- Monster al que se le calcula la estadistica
     -> (Stats -> Int)  -- Estadistica a calcular
     -> Int
stat mon f = floor $ 5 + ((*) mLVL $ mIV + 2 * mBase + mEV / 4) / 100
  where
    mLVL  = fromIntegral.lvl $ mon
    mIV   = fromIntegral.f.iv $ mon
    mEV   = fromIntegral.f.ev $ mon
    mBase = fromIntegral.f.base.species $ mon

-- Determina el dano resultante de que un Pokemon use un ataque a otro Pokemon
damage :: Move          -- Move que usara el Pokemon
       -> Monster       -- Monster que ataca
       -> Monster       -- Monster que defiende
       -> Int
damage mov atk def = floor $ (*) modifier $ ((2 * aLVL + 10) / 250) * (aAtk / dDef) * mBase + 2
  where
    aLVL     = fromIntegral.lvl $ atk
    aAtk     = fromIntegral.(if physical mov then attack else spAttack).stats $ atk
    dDef     = fromIntegral.(if physical mov then defense else spDefense).stats $ def
    mBase    = fromIntegral.power $ mov
    modifier = 
      let
        stab    = if elem (moveType mov) $ pokeType.species $ atk then 1.5 else 1
        typeSE  = fromIntegral $ max 1 $ (*) 2 $ length $ intersect ((\(x,_,_) -> x) relation) $ dType
        typeNVE = (/) 1 $ fromIntegral $ max 1 $ (*) 2 $ length $ intersect ((\(_,x,_) -> x) relation) $ dType
        typeI   = if null $ intersect ((\(_,_,x) -> x) relation) dType then 1 else 0
      in stab * typeSE * typeNVE * typeI
        where
          relation = moveTypeRelation.moveType $ mov
          dType    = pokeType.species $ def