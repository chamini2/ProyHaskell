{-
 -   Módulo: PokeBattle.hs
 -
 -   Integrantes: Grupo 16
 -   Alberto Cols      09-10177
 -   Matteo Ferrando   09-10285
 -
 -   Se encuentran todas las funciones necesarias para la simulación
 -   de la batalla así como los data utilizados.
 -}

module PokeBattle 
  ( Result(..)
  , battle
  ) where

import Data.Tuple (swap)
import Data.Char (isDigit)
import System.IO (putStrLn)
import System.Environment (getArgs)

import Data.List.Split (splitOn)

import Pokemon

-- Resultados finales posibles para una batalla
data Result 
  = Trainer1
  | Trainer2 
  | Draw 
  deriving (Show, Eq)

-- Comandos de accion posibles a elegir por un entrenador
data Command
  = FIGHT Int 
  | PKMN Int 
  | RUN 
  deriving (Eq)

-- Obtiene le Monster en uso del Trainer
getActive :: Trainer -> Monster
getActive trainer = pokeballs trainer !! active trainer

-- Indica si un Pokemon no puede pelear mas
fainted :: Monster -> Bool
fainted poke = presHP poke <= 0

-- Indica si todos los Pokemon de un Trainer no estan en capacidad de pelear
defeated :: Trainer -> Bool
defeated trainer = all fainted $ pokeballs trainer

-- Debilita el HP del Pokemon defensor
-- y decrementa el PP del Move usado por el Pokemon atacante
attacks :: Int -> Trainer -> Trainer -> (Trainer,Trainer)
attacks movN atk def = 
  ( atk { pokeballs = map modifyAtk $ pokeballs atk }
  , def { pokeballs = map modifyDef $ pokeballs def }
  )
  where
  modifyAtk x
    | x == actA = x { moves = map modifyMove $ moves actA }
    | otherwise = x
  modifyMove y 
    | y == mov  = y { monPP = (monPP mov - 1) }
    | otherwise = y
  modifyDef x 
    | x == actD = 
      x { presHP = 
        if dmg <= (presHP actD) 
        then (presHP actD - dmg) 
        else 0 }
    | otherwise = x
  mov  = (moves actA) !! movN
  actA = getActive atk
  actD = getActive def
  dmg  = damage (monMove mov) (getActive atk) (getActive def)

-- Cambia el Pokemon activo del Trainer pasado, 
-- en caso de que el Trainer haya pedido el cambio
pkmn :: Trainer -> Command -> Trainer
pkmn trainer (PKMN x) = trainer {active = x}
pkmn trainer _        = trainer

-- Determina que Pokemon atacara primero por su speed 
-- y lleva a cabo los ataques correspondientes.
checkSpeed :: (Trainer, Int) -> (Trainer, Int) -> (Trainer,Trainer)
checkSpeed (left, l) (right, r)
  | getSpeed left >= getSpeed right = 
    let 
      (nLeft, nRight) = attacks l left right
    in 
      if fainted $ getActive nRight 
      then (nLeft, nRight)
      else swap $ attacks r nRight nLeft
  | otherwise                       =
    let 
      (nRight, nLeft) = attacks r right left
    in 
      if fainted $ getActive nLeft 
      then (nLeft, nRight)
      else attacks l nLeft nRight
  where
    getSpeed trainer = speed $ stats $ getActive trainer

-- Formato de impresion del estado de un Monster
activeStatus :: Monster -> String
activeStatus mon = nickname mon ++ " | LVL" ++ (show.lvl $ mon) ++ " | "
                   ++ (show.presHP $ mon) ++ "/" ++ (show.hp.stats $ mon) ++ "HP"

-- Un String listando a los Pokemon de un Trainer con su numero de invocacion
pokeList :: Trainer -> String
pokeList trainer = foldr format "" $ zip [1..] $ map tupleFormat $ pokeballs trainer
  where
  format (x, (nam, lvl, hp, pres, faint)) y = 
    show x ++ " -> " ++ nam ++ " LVL" ++ show lvl ++ " " ++ show hp ++ "/" 
    ++ show pres ++ "HP" ++ (if faint then " FAINTED" else "") ++ "\n" ++ y
  tupleFormat x                             =
    (nickname x, lvl x, presHP x,hp.stats $ x, fainted x)

-- Lleva a cabo la batalla entera, devolviendo el resultado de esta
battle :: Trainer -> Trainer -> IO Result
battle left right = do  
  putStrLn $ "Trainer1's\n\t" ++ (activeStatus.getActive $ left)
  putStrLn $ "Trainer2's\n\t" ++ (activeStatus.getActive $ right) ++ "\n"
  case (defeated left, defeated right) of
    (True, True) -> return Draw
    (_, True)    -> return Trainer1
    (True, _)    -> return Trainer2
    otherwise    -> do
      -- Si el pokemon actual esta fainted, lo cambiamos a uno activo
      aLeft  <- chooseActive 1 left
      aRight <- chooseActive 2 right
      -- Pido un comando de accion
      comm1  <- turn aLeft right
      comm2  <- turn aRight left
      -- En caso de cambio de pokemon
      let nLeft  = pkmn aLeft comm1 
      let nRight = pkmn aRight comm2
      -- Posibles combinaciones de flujo de batalla
      case (comm1, comm2) of
        (RUN, RUN)         -> return Draw
        (_, RUN)           -> return Trainer1
        (RUN, _)           -> return Trainer2
        (PKMN x, PKMN y)   -> battle nLeft nRight
        (PKMN x, FIGHT y)  -> uncurry battle $ swap $ attacks y nRight nLeft
        (FIGHT x, PKMN y)  -> uncurry battle $ attacks x nLeft nRight
        (FIGHT x, FIGHT y) -> uncurry battle $ checkSpeed (nLeft,x) (nRight,y)

-- Para obligar al Trainer a elegir un Pokemon que no este debilitado 
-- a punto de demasyo, en caso de que el actualmente activo lo este
chooseActive :: Int -> Trainer -> IO Trainer
chooseActive n trainer
  | fainted $ getActive trainer = do
    putStrLn
      $ "Trainer" ++ show n ++ "! " ++ (nickname.getActive $ trainer) 
      ++ " can't battle anymore!! Choose a new Pokemon to battle!"
    putStrLn $ pokeList trainer
    input <- getLine
    putStrLn "\n"
    let newP = (read input :: Int) - 1
    let nTrainer = trainer { active = 
      if (0 <= newP) 
          && ((<) newP $ length $ pokeballs trainer) 
      then newP 
      else active trainer }
    chooseActive n nTrainer
  | otherwise                   = return trainer

-- Pide comandos al Trainer hasta que pida uno de accion
turn :: Trainer -> Trainer -> IO Command
turn atk def = do
  putStrLn $ "What will " ++ (nickname.getActive $ atk) ++ " do!?"
  putStrLn "================================="
  putStrLn "| FIGHT N         -      PKMN N |"
  putStrLn "| INFO [ME|RIVAL] -  HELP - RUN |"
  putStrLn "=================================\n"
  input <- getLine
  putStrLn "\n"
  let 
    (x:xs)  = splitOn " " input
    invalid str = do 
      putStrLn $ "Invalid " ++ str ++ "...\n"
      turn atk def
   in 
    if length (x:xs) == 1 then
      case x of
        "ayuda"    -> do
          putStrLn $ help atk
          turn atk def
        "rendirse" -> return RUN
        otherwise  -> invalid "option"
    else if length (x:xs) == 2 then
      case x of
        "atacar"  -> 
          let 
            n = (read (head xs) :: Int) - 1
          in
            if all isDigit (head xs) && (head xs) /= ""
            then 
              if (0 <= n) && ((<) n $ length.moves.getActive $ atk) 
                  && (0 < monPP ((moves.getActive $ atk) !! n))
              then return $ FIGHT n
              else invalid "move"
            else invalid "option"
        "cambiar" -> 
          let 
            n = (read (head xs) :: Int) - 1
          in 
            if all isDigit (head xs) && (head xs) /= ""
            then 
              if (n /= active atk) 
                  && (0 <= n) && ((<) n $ length $ pokeballs atk) 
                  && (not $ fainted $ (pokeballs atk) !! n)
              then return $ PKMN n
              else invalid "pokemon"
            else invalid "option"
        "info"    -> 
          case (head xs) of
            "yo"      -> do 
              putStrLn $ info atk
              turn atk def
            "rival"   -> do 
              putStrLn $ info def
              turn atk def
            otherwise -> invalid "info"
        otherwise -> invalid "option"
    else invalid "option"
  where
    info trainer = show.species.getActive $ trainer
    help trainer = (show.getActive $ trainer) ++ pokeList atk