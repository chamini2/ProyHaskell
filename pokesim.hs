module Main (main) where
import qualified Data.ByteString as Str
import Data.List.Split (splitOn)
import System.IO
import System.Environment
import Pokemon


main = do
        args <- getArgs
        putStrLn "Let's PokeBattle!"
        print args
        if length args /= 4 then putStrLn ("Usage: pokemons.csv moves.csv trainer1.csv trainer2.csv") 
        else
          do
          speciesFile <- readFile (args !! 0)
          movesFile   <- readFile (args !! 1)
          train1File  <- readFile (args !! 2)
          train2File  <- readFile (args !! 3)
          print (filter ("" /=) $ lines speciesFile)
          putStrLn "\n\n\n\n\n"
          print (filter ("" /=) $ lines movesFile)
          putStrLn "\n\n\n\n\n"
          print (filter ("" /=) $ lines train1File) 
          putStrLn "\n\n\n\n\n"
          print (filter ("" /=) $ lines train2File) 
          putStrLn "\n\n\n\n\n"
          let species  = speciesParse (filter ("" /=) $ lines speciesFile)
          let moves    = movesParse   (filter ("" /=) $ lines movesFile)
          let trainer1 = trainerParse (filter ("" /=) $ lines train1File) species moves
          let trainer2 = trainerParse (filter ("" /=) $ lines train2File) species moves
          putStrLn $ "\nSPECIES\n" ++ speciesFile ++ "\n"
          print species
          putStrLn $ "\nMOVES\n" ++ movesFile ++ "\n"
          print moves
          putStrLn $ "\nTRAINER 1\n" ++ train1File ++ "\n"
          --print trainer1
          putStrLn $ "\nTRAINER 2\n" ++ train2File ++ "\n"
          print trainer2
          putStrLn "Ataque"
          ataque <- getLine
          let ataque = "tackle"
          print $ damage ((filter (\m -> moveName m == ataque) moves) !! 0) ((snd trainer1) !! 0) ((snd trainer2) !! 0)


speciesParse :: [String] -> [Species]
speciesParse = map (\p -> species $ splitOn "," p)
  where
    species p = 
      Species { no       = read (p !! 0) :: Int
              , name     = p !! 1
              , pokeType = (read (p !! 2) :: Type) 
                            : (if (p !! 3) /= "" then (read (p !! 3) :: Type) : [] else [])
              , base     = 
                  Stats { hp        = read (p !! 4) :: Int
                        , attack    = read (p !! 5) :: Int
                        , defense   = read (p !! 6) :: Int
                        , spAttack  = read (p !! 7) :: Int
                        , spDefense = read (p !! 8) :: Int
                        , speed     = read (p !! 9) :: Int
                        }
              , preEvolution = if (p !! 10) /= "" then Just (read (p !! 10) :: Int, p !! 11) else Nothing
              , evolutions   = []
              }

movesParse :: [String] -> [Move]
movesParse = map (\p -> moves $ splitOn "," p)
  where
    moves p = 
      Move { moveName = p !! 0
           , moveType = read (p !! 1) :: Type
           , physical = read (p !! 2) :: Bool
           , pp       = read (p !! 3) :: Int
           , power    = read (p !! 4) :: Int
           }

trainerParse :: [String] -> [Species] -> [Move] -> Trainer
trainerParse x specs moves =  (0, map (\p -> monster $ splitOn "," p) x)
  where
    monster p = 
      mons { presHP = maxHP mons
           , stats  = Stats { hp        = maxHP mons
                            , attack    = stat mons attack
                            , defense   = stat mons defense
                            , spAttack  = stat mons spAttack
                            , spDefense = stat mons spDefense
                            , speed     = stat mons speed
                            }
           }
      where
        mons = 
          Monster { species  = pokeSpecies
                  , nickname = p !! 1
                  , lvl      = read (p !! 2) :: Int
                  , presHP   = 0 
                  , moves    = getMoves $ filter ("" /=) [p !! 3, p !! 4, p !! 5, p !! 6]
                  , stats    = base pokeSpecies
                  , iv       = perfIV
                  , ev       = perfEV
                  }
          where
            pokeSpecies = (filter (\s -> no s == (read (p !! 0) :: Int)) specs) !! 0
            getMoves = map (\n -> (move n, pp $ move n))
              where
                move n = ((filter (\m -> moveName m == n) moves) !! 0)
            perfIV = 
              Stats { hp  = 31
                    , attack    = 31
                    , defense   = 31
                    , spAttack  = 31
                    , spDefense = 31
                    , speed     = 31
                    }
            perfEV = 
              Stats { hp  = 255
                    , attack    = 255
                    , defense   = 255
                    , spAttack  = 255
                    , spDefense = 255
                    , speed     = 255
                    }