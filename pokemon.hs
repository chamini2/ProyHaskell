import Data.List

{- 
 - Definicion de Type, los posibles Tipos para un Monstruo o Ataque
 -}
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

{- 
 - Definicion de Stats, todas las estadisticas de un Pokemon
 -}
data Stats = Stats { hp        :: Int
                   , attack    :: Int
                   , defense   :: Int
                   , spAttack  :: Int
                   , spDefense :: Int
                   , speed     :: Int
                   } deriving (Show)

{- 
 - Definicion de Species, los datos relevantes para una Especie
 -}
data Species = Species { no           :: Int
                       , name         :: String
                       , pokeType     :: [Type]
                       , base         :: Stats
                       , preEvolution :: Maybe Int
                       , evolution    :: [Int]
                       } deriving (Show)

{- 
 - Definicion de Move, los datos de un Ataque
 -}
data Move = Move { moveName :: String
                 , moveType :: Type
                 , physical :: Bool
                 , pp       :: Int
                 , power    :: Int
                 } deriving (Show)

{-
 - Para llevar cuantos PP le quedan a esta movida para este Monstruo
 -}
type MonsterMove = (Move, Int)

{- 
 - Definicion de Monster, un Monstruo especifico de cierta Especie
 -}
data Monster = Monster { species  :: Species
                       , nickname :: String
                       , lvl      :: Int
                       , presHP   :: Int
                       , moves    :: [MonsterMove]
                       , iv       :: Stats
                       , ev       :: Stats
                       } deriving (Show)

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

{-
 - Funcion que calcula el HP maximo actual para un Monstruo
 -}

-- el internet dice que se hace realmente -> (div ((hp . ev) x) 4)
maxHP :: Monster -> Int
maxHP x =  round $ (((hp . iv) x + 2 * (hp . base . species) x + (div ((hp . ev) x) 4) + 100) * (lvl x)) /. 100 + 10
  where
    x /. y = fromIntegral x / fromIntegral y

{- 
 - Funcion que calcula una estadistica actual para un Monstruo
 -}
statAttack :: Monster -> Int
statAttack x = div (((attack . iv) x + 2 * (attack . base . species) x + div ((attack . ev) x) 4) * (lvl x)) 100 + 5

{- 
 - Funcion que calcula una estadistica actual para un Monstruo
 -}
statDefense :: Monster -> Int
statDefense x = div (((defense . iv) x + 2 * (defense . base . species) x + div ((defense . ev) x) 4) * (lvl x)) 100 + 5

{- 
 - Funcion que calcula el dano de un Ataque
 -}
damage :: Move -> Monster -> Monster -> Int
damage move atk def = round $ fromIntegral ((div (2 * lvl atk + 10) {-/.-} 250) * (div (statAttack atk) {-/.-} (statDefense def)) * power move + 2) * modifier
  where
    x /. y = fromIntegral x / fromIntegral y
    modifier = (if elem (moveType move) ((pokeType . species) atk) then 1.5 else 1) -- STAB
             * (fromIntegral ( max 1 ((length (filter (moveType move == ) (concat (map ((\(x,_,_) -> x) . moveTypeRelation) ((pokeType . species) def))))) * 2)))
             * (max 1 (fromIntegral (length (filter (moveType move == ) (concat (map ((\(_,y,_) -> y) . moveTypeRelation) ((pokeType . species) def))))) / 2))
             * (if elem (moveType move) (concat (map ((\(_,_,z) -> z) . moveTypeRelation) ((pokeType . species) def))) then 0 else 1)




