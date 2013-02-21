module Main (main) where
import Data.List.Split (splitOn)
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import System.IO
import System.Environment
import New

main = do
        args <- getArgs
        putStrLn "Let's PokeBattle!"
        print args
        if length args /= 4 then putStrLn "Usage: pokemons.csv moves.csv trainer1.csv trainer2.csv"
        else
          do
          speciesFile <- readFile (args !! 0)
          movesFile   <- readFile (args !! 1)
          train1File  <- readFile (args !! 2)
          train2File  <- readFile (args !! 3)
          print speciesFile
          print movesFile 
          print train1File 
          print train2File 
          let species  = speciesParse $ linesParse speciesFile
          let moves    = movesParse   $ linesParse movesFile
          let trainer1 = trainerParse (linesParse train1File) species moves
          let trainer2 = trainerParse (linesParse train2File) species moves
          print species
          print moves
          print trainer1
          print trainer2

linesParse :: String -> [[String]]
linesParse s = map (splitOn (",")) $ filter ("" /=) $ lines s

speciesParse :: [[String]] -> [Species]
speciesParse list = map getEvolutions $ map species list
  where
    species [sNo, sName, sPT1, sPT2, sHP, sAtk, sDef, sSpAtk, sSpDef, sSpd, sPreE, sCr] = 
      Species { no       = read sNo :: Int
              , name     = sName
              , pokeType = (read sPT1 :: Type) 
                            : (if sPT2 /= "" then (read sPT2 :: Type) : [] else [])
              , base     = 
                  Stats { hp        = read sHP    :: Int
                        , attack    = read sAtk   :: Int
                        , defense   = read sDef   :: Int
                        , spAttack  = read sSpAtk :: Int
                        , spDefense = read sSpDef :: Int
                        , speed     = read sSpd   :: Int
                        }
              , preEvolution = if sPreE /= "" then Just (read sPreE :: Int, sCr) else Nothing
              , evolutions   = []
              }
    getEvolutions :: Species -> Species
    getEvolutions x      = 
      x { evolutions = let pre = fromJust.preEvolution
                        in map (\y -> (no y, snd.pre $ y)) 
                            $ filter ((no x==).fst.pre) 
                              $ filter (isJust.preEvolution)
                                $ map species list
        }

movesParse :: [[String]] -> [Move]
movesParse = map (\p -> moves p)
  where
    moves [mName, mType, mPhy, mPP, mPWR] = 
      Move { moveName = mName
           , moveType = read mType :: Type
           , physical = read mPhy  :: Bool
           , pp       = read mPP   :: Int
           , power    = read mPWR  :: Int
           }

trainerParse :: [[String]] -> [Species] -> [Move] -> Trainer
trainerParse x specs moves =  (0, map monster x)
  where
    monster [pSpec, pNick, pLVL, pAtk1, pAtk2, pAtk3, pAtk4] = 
      mons { presHP = maxHP mons
           , stats  = 
              Stats { hp        = maxHP mons
                    , attack    = stat  mons attack
                    , defense   = stat  mons defense
                    , spAttack  = stat  mons spAttack
                    , spDefense = stat  mons spDefense
                    , speed     = stat  mons speed
                    }
           }
      where
        mons = 
          Monster { species  = pokeSpecies
                  , nickname = pNick
                  , lvl      = read pLVL :: Int
                  , presHP   = 0 
                  , moves    = getMoves $ filter ("" /=) [pAtk1, pAtk2, pAtk3, pAtk4]
                  , stats    = base pokeSpecies
                  , iv       = perfIV
                  , ev       = perfEV
                  }
          where
            pokeSpecies = fromJust $ find (\s -> no s == (read pSpec :: Int)) specs
            getMoves = map (\n -> (move n, pp $ move n))
              where
                move n = ((filter (\m -> moveName m == n) moves) !! 0)
            perfIV = Stats { hp = 31  , attack = 31  , defense = 31  , spAttack = 31  , spDefense = 31  , speed = 31  }
            perfEV = Stats { hp = 255 , attack = 255 , defense = 255 , spAttack = 255 , spDefense = 255 , speed = 255 }