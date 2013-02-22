{-
 -   Módulo: PokeSim.hs
 -
 -   Integrantes: Grupo 16
 -   Alberto Cols      09-10177
 -   Matteo Ferrando   09-10285
 -
 -   Se encuentra el Main del proyecto.
 -   
 -   Lee por archivo la información de todos los Pokemons, 
 -   los movimientos y los dos entrenadores.
 - 
 -   Luego simula la batalla entre dichos entrenadores.
 -}

module Main (main) where

import System.IO (putStrLn)
import System.Environment (getArgs)

import Pokemon
import PokeParse
import PokeBattle

main = do
  args <- getArgs
  putStrLn " ======================================================================================="
  putStrLn "|| atacar = FIGHT, cambiar = PKMN, info = INFO, rendirse = RUN, yo = ME, rival = RIVAL ||"
  putStrLn " ======================================================================================="
  putStrLn "Let's PokeBattle!\n"
  if length args /= 4 
  then putStrLn "Usage: pokemons.csv moves.csv trainer1.csv trainer2.csv"
  else do
    -- Inicializando listas desde los archivos a usar en el programa entero
    speciesFile <- readFile $ args !! 0
    movesFile   <- readFile $ args !! 1
    train1File  <- readFile $ args !! 2
    train2File  <- readFile $ args !! 3
    let species  = speciesParse $ linesParse speciesFile
    let moves    = movesParse   $ linesParse movesFile
    let trainer1 = trainerParse (linesParse train1File) species moves
    let trainer2 = trainerParse (linesParse train2File) species moves
    -- Simulacion de la batalla
    winner <- battle trainer1 trainer2
    case winner of
      Trainer1 -> do 
        putStrLn $ "\nUnbelievable! Trainer1 defeated Trainer2!\n"
      Trainer2 -> do 
        putStrLn $ "\nUnbelievable! Trainer2 defeated Trainer1!\n"
      Draw     -> do 
        putStrLn "\nIt's a draw!\n"