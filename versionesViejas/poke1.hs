module Pokemon where
--No exportamos las funciones
import Data.List

type Trainer = ( Int        --Entero que indica cual posicion de la lista de Pokemones es el actual
               , [Monster]  --Lista de Pokemones de un entrenador
               )

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
data Stats = Stats 
              { hp        :: Int
              , attack    :: Int
              , defense   :: Int
              , spAttack  :: Int
              , spDefense :: Int
              , speed     :: Int
              } deriving (Show)

{-
 - Tipo para definir lo relevante a una evolucion
 -}
type Evolution = (Int        --Indicador de numero en Pokedex
                 ,String     --Criterio de evolucion
                 )

{- 
 - Definicion de Species, los datos relevantes para una Especie
 -}
data Species = Species 
                { no           :: Int
                , name         :: String
                , pokeType     :: [Type]
                , base         :: Stats
                , preEvolution :: Maybe Evolution
                , evolutions   :: [Evolution]
                } deriving (Show)

{- 
 - Definicion de Move, los datos de un Ataque
 -}
data Move = Move 
              { moveName :: String
              , moveType :: Type
              , physical :: Bool
              , pp       :: Int
              , power    :: Int
              }

instance Show (Move) where
  show (Move moveName moveType physical pp power) = 
    "{" ++ show moveName ++ ", " ++ show moveType 
    ++ (if physical then ", Physical" else ", Special") ++
    ", " ++ show pp ++ "PP, PWR " ++ show power ++ "}\n"

{-
 - Para llevar cuantos PP le quedan a esta movida para este Monstruo
 -}
type MonsterMove = ( Move     --Ataque del Monstruo
                   , Int      --PP restantes para este Ataque de este Monstruo
                   )

{- 
 - Definicion de Monster, un Monstruo especifico de cierta Especie
 -}
data Monster = Monster 
                { species  :: Species
                , nickname :: String
                , lvl      :: Int
                , presHP   :: Int
                , moves    :: [MonsterMove]
                , stats    :: Stats
                , iv       :: Stats
                , ev       :: Stats
                }

instance Show (Monster) where
  show (Monster species nickname lvl presHP moves stats iv ev) = 
    "Species: (" ++ show (no species) ++ ", " ++ show (name species)
    ++ ")\nNickname: " ++ show nickname ++ "\nLVL: " ++ show lvl
    ++ "\nStats\n  HP: " ++ show presHP ++ "/" ++ show (hp stats)
    ++ "\n  Attack: " ++ show (attack stats) ++ "\n  Defense: "
    ++ show (defense stats) ++ "\n  SpAttack: " ++ show (spAttack stats)
    ++ "\n  SpDefense: " ++ show (spDefense stats) ++ "\n  Speed: "
    ++ show (speed stats) ++ "\nMoves\n" ++ show (map (fst) moves) ++ "\n"

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
maxHP :: Monster -> Int
maxHP x =  div (((hp . iv) x + 2 * (hp . base . species) x + (div ((hp . ev) x) 4) + 100) * (lvl x)) 100 + 10

{- 
 - Funcion que calcula una estadistica actual para un Monstruo
 -}
stat :: Monster -> (Stats -> Int) -> Int
stat x f = div (((f . iv) x + 2 * (f . base . species) x + div ((f . ev) x) 4) * (lvl x)) 100 + 5

{- 
 - Funcion que calcula el dano de un Ataque
 -}
damage :: Move -> Monster -> Monster -> Int
damage move atk def = round $ ((fI (2 * (lvl atk) + 10) / 250)                             --La parte del nivel
                        * (fI (stat atk (if physical move then attack else spAttack)) 
                          / fI (stat def (if physical move then defense else spDefense)))  --La parte de las estadisticas
                        * fI (power move) + 2) * modifier                                  --Poder del ataque y modificador
  where
    fI = fromIntegral
    relation = moveTypeRelation . moveType $ move
    defType  = pokeType . species $ def
    modifier = (if elem (moveType move) $ (pokeType . species) atk then 1.5 else 1) -- STAB
             * fI (max 1 $ (2 * (length $ intersect ((\ (x,_,_) -> x) relation) defType)))
             * (1 / fI (max 1 $ (2 * (length $ intersect ((\ (_,y,_) -> y) relation) defType))))
             * (if null $ intersect ((\ (_,_,z) -> z) relation) defType then 1 else 0)