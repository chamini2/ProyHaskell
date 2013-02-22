{-
 -   Módulo: Pokemon.hs
 -
 -   Integrantes: Grupo 16
 -   Alberto Cols      09-10177
 -   Matteo Ferrando   09-10285
 -
 -   Se definen todas las estructuras que definen a un Pokemon, así como las 
 -   funciones para obtener sus Stats y de las acciones que realizan.
 -}

module Pokemon
  ( Type(..)
  , Stats(..)
  , Evolution(..)
  , Species(..)
  , Move(..)
  , Monster(..)
  , activeStatus
  , MonsterMove(..)
  , Trainer(..)
  , TrainerMonster
  , maxHP
  , stat
  , damage
  ) where

import Data.Maybe (isJust, fromJust)
import Data.List (intersect)

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
    } 
  deriving (Eq, Read)

instance Show (Stats) where
  show (Stats hp attack defense spAttack spDefense speed) = 
    "\tHP " ++ show hp ++ "\n\tAttack  " ++ show attack 
    ++ "\n\tDefense " ++ show defense ++ "\n\tSpAttack " 
    ++ show spAttack  ++ "\n\tSpDefense " ++ show spDefense 
    ++ "\n\tSpeed " ++ show speed

-- Evolucion de un Pokemon a otro Pokemon
data Evolution = 
  Evolution
    { eNo      :: Species  -- Indicador de numero en Pokedex
    , criterio :: String   -- Criterio de evolucion
    } 
  deriving (Eq, Read, Show)

-- Datos relevantes para una especie de un Pokemon
data Species = 
  Species
    { no           :: Int
    , name         :: String
    , pokeType     :: [Type]
    , base         :: Stats
    , preEvolution :: Maybe Evolution
    , evolutions   :: [Evolution]
    } 
  deriving (Eq, Read)

instance Show (Species) where
  show (Species num sName pokeType base preEvolution evolutions) = 
    "PokeDex " ++ show num ++ "\nName    " ++ sName ++ "\nType    "
    ++ show pokeType ++ "\nBase\n" ++ show base 
    ++ (if isJust preEvolution 
        then "\nPreEvolution\n" ++ (flip format "" $ tupleFormat.fromJust $ preEvolution) 
        else "")
    ++ (if not.null $ evolutions 
        then "\nEvolutions\n" ++ (foldr format "" $ map tupleFormat $ evolutions) ++ "\n"
        else "")
    where
    format (num, nam, cr) y = "\t" ++ (show num) ++ " - " ++ nam ++ " - " ++ cr ++ "\n" ++ y
    tupleFormat :: Evolution -> (Int, String, String)
    tupleFormat x     = (no.eNo $ x, name.eNo $ x, criterio x)

-- Representa un ataque
data Move = 
  Move
    { moveName :: String
    , moveType :: Type
    , physical :: Bool
    , pp       :: Int
    , power    :: Int
    } 
  deriving (Eq, Read, Show)

-- Para asociar un ataque con un monstruo, con los PP que le quedan disponibles
data MonsterMove = 
  MonsterMove 
    { monMove :: Move   -- Move del Monster
    , monPP   :: Int    -- PP restantes para este Move de este Monster
    } 
  deriving (Eq, Read)

instance Show (MonsterMove) where
  show (MonsterMove monMove monPP) = "\t" ++ (moveName monMove) ++ " " ++ show monPP ++ "PP\n"

-- Representa un monstruo particular
data Monster = 
  Monster 
    { species  :: Species
    , nickname :: String
    , lvl      :: Int
    , presHP   :: Int
    , moves    :: [(Int, MonsterMove)]
    , stats    :: Stats
    , iv       :: Stats
    , ev       :: Stats
    } 
  deriving (Eq, Read)

instance Show (Monster) where
  show (Monster species nickname lvl presHP moves stats iv ev) = 
    nickname ++ "\nMove(s)\n" ++ (foldr format "" $ zip [1..] $ map tupleFormat $ moves)
    where
    format (x, (mov, pp)) y = "\t" ++ show x ++ " -> " ++ mov ++ " " ++ (show pp) ++ "PP\n" ++ y
    tupleFormat x           = (moveName.monMove.snd $ x, monPP.snd $ x)

-- Formato de impresion del estado de un Monster
activeStatus :: Monster -> String
activeStatus mon = nickname mon ++ " | LVL" ++ (show.lvl $ mon) ++ " | "
                   ++ (show.presHP $ mon) ++ "/" ++ (show.hp.stats $ mon) ++ "HP"


-- Para indicar en que pokebola esta el Monster
type TrainerMonster = (Int,Monster)

-- Lista de Pokemones de un entrenador y un indicador de cual esta en uso ahora
data Trainer = 
  Trainer 
    { active     :: Int         -- Entero que indica cual posicion de la lista de Pokemones es el actual
    , pokeballs  :: [TrainerMonster]   -- Lista de Pokemones de un entrenador
    } 
  deriving (Eq, Read, Show)

-- Determina, para un Type de ataque, cuales tipos son super efectivos,
-- cuales tipos son resistentes y cuales son inmunes.
moveTypeRelation :: Type      -- Type de ataque a determinar la relación.
                 -> ( [Type]  -- | Type es super efectivo a [Type] (2x dano)
                    , [Type]  -- | Type ataca con resistencia a [Type] (0.5x dano)
                    , [Type]  -- | Type no le hace dano a Pokemones de [Type] (0x dano)
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
     -> Int             -- Valor de la estadistica total
stat mon f = floor $ 5 + ((*) mLVL $ mIV + 2 * mBase + mEV / 4) / 100
  where
  mLVL  = fromIntegral.lvl $ mon
  mIV   = fromIntegral.f.iv $ mon
  mEV   = fromIntegral.f.ev $ mon
  mBase = fromIntegral.f.base.species $ mon

-- Determina el dano resultante de que un Pokemon use un ataque a otro Pokemon
damage :: Move          -- Move que usara el Pokemon
       -> Monster       -- Monster que ataca
       -> Monster       -- Monster defensor
       -> Int           -- Dano a realizar al Monster defensor
damage mov atk def = floor $ (*) modifier $ ((2 * aLVL + 10) / 250) * (aAtk / dDef) * mBase + 2
  where
  aLVL     = fromIntegral.lvl $ atk
  aAtk     = fromIntegral.(if physical mov then attack else spAttack).stats $ atk
  dDef     = fromIntegral.(if physical mov then defense else spDefense).stats $ def
  mBase    = fromIntegral.power $ mov
  modifier = 
    let
      stab    = if elem (moveType mov) $ pokeType.species $ atk then 1.5 else 1
      typeSE  = fromIntegral $ max 1 $ (*) 2 $ length 
                  $ intersect (first relation) dType
      typeNVE = (/) 1 $ fromIntegral $ max 1 $ (*) 2 $ length 
                  $ intersect (secnd relation) dType
      typeI   = if null $ intersect (third relation) dType then 1 else 0
    in stab * typeSE * typeNVE * typeI
      where
      relation = moveTypeRelation.moveType $ mov
      dType    = pokeType.species $ def
      first (x,_,_) = x
      secnd (_,y,_) = y
      third (_,_,z) = z