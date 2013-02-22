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
import Data.Maybe (fromJust)
import Data.List (find)

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
getActive :: Trainer -> TrainerMonster
getActive trainer = fromJust $ find ((active trainer==).fst) $ pokeballs trainer

-- Indica si un Pokemon no puede pelear mas
fainted :: TrainerMonster -> Bool
fainted poke = (presHP.snd $ poke) <= 0

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
  modifyAtk (i, m)
    | i == iA   = (i, m { moves = map modifyMove $ moves mA })
    | otherwise = (i, m)
  modifyMove (i, m) 
    | i == iM   = (i, m { monPP = (monPP mov - 1) })
    | otherwise = (i, m)
  modifyDef (i, m)
    | i == iD   = 
      ( i
      , m { presHP = 
          if dmg <= (presHP mD) 
          then (presHP mD - dmg) 
          else 0 
        }
      )
    | otherwise = (i, m)
  (iM, mov) = fromJust $ find ((movN==).fst) $ moves mA
  (iA, mA)  = getActive atk
  (iD, mD)  = getActive def
  dmg       = damage (monMove mov) mA mD

-- Cambia el Pokemon activo del Trainer pasado, 
-- en caso de que el Trainer haya pedido el cambio
pkmn :: Trainer -> Command -> Trainer
pkmn trainer (PKMN x) = trainer { active = x }
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
    getSpeed trainer = speed.stats.snd.getActive $ trainer

-- Un String listando a los Pokemon de un Trainer con su numero de invocacion
pokeList :: Trainer -> String
pokeList trainer = foldr format "" $ zip [1..] $ map tupleFormat $ pokeballs trainer
  where
  format (x, (nam, lvl, hp, pres, faint)) y = 
    show x ++ " -> " ++ nam ++ " LVL" ++ show lvl ++ " " ++ show hp ++ "/" 
    ++ show pres ++ "HP" ++ (if faint then " FAINTED" else "") ++ "\n" ++ y
  tupleFormat (i,m)                         =
    (nickname m, lvl m, presHP m,hp.stats $ m, fainted (i,m))

-- Lleva a cabo la batalla entera, devolviendo el resultado de esta
battle :: Trainer -> Trainer -> IO Result
battle left right = do  
  putStrLn $ "Trainer1's\n\t" ++ (activeStatus.snd.getActive $ left)
  putStrLn $ "Trainer2's\n\t" ++ (activeStatus.snd.getActive $ right) ++ "\n"
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
      $ "Trainer" ++ show n ++ "! " ++ (nickname.snd.getActive $ trainer) 
      ++ " can't battle anymore!! Choose a new Pokemon to battle!"
    putStrLn $ pokeList trainer
    input <- getLine
    putStrLn "\n"
    let comando = splitOn " " input
    if (length comando == 2)
    then
      let 
        [com,num] = comando
      in
        if (com == "cambiar") && (all isDigit num) && num /= ""
        then do
          let newP     = read num :: Int
          let nTrainer = trainer { active = 
            if (1 <= newP) 
                && ((<=) newP $ length $ pokeballs trainer) 
            then newP 
            else active trainer }
          chooseActive n nTrainer
        else chooseActive n trainer
    else chooseActive n trainer
  | otherwise                   = return trainer

-- Pide comandos al Trainer hasta que pida uno de accion
turn :: Trainer -> Trainer -> IO Command
turn atk def = do
  putStrLn $ "What will " ++ (nickname.snd.getActive $ atk) ++ " do!?"
  putStrLn "================================="
  putStrLn "| FIGHT N         -      PKMN N |"
  putStrLn "| INFO [ME|RIVAL] -  HELP - RUN |"
  putStrLn "=================================\n"
  input <- getLine
  putStrLn "\n"
  let 
    invalid str = do 
      putStrLn $ "Invalid " ++ str ++ "...\n"
      turn atk def
   in 
    case splitOn " " input of
      -- Un solo elemento
      [x]       ->
        case x of
          "ayuda"    -> do
            putStrLn $ help atk
            turn atk def
          "rendirse" -> return RUN
          otherwise  -> invalid "option"
      -- Dos elementos
      [x,y]     ->
        let
          n = read y :: Int
        in
          case x of
            "atacar"  -> 
              if all isDigit y && y /= ""
              then 
                if (1 <= n) && ((<=) n $ length.moves.snd.getActive $ atk) 
                    && ((<=) 0 $ monPP.snd.fromJust 
                        $ find ((n==).fst) $ moves.snd.getActive $ atk)
                then return $ FIGHT n
                else invalid "move"
              else invalid "option"

            "cambiar" -> 
              if all isDigit y && y /= ""
              then 
                if (n /= active atk) 
                    && (1 <= n) && ((<=) n $ length.pokeballs $ atk) 
                    && (not.fainted.fromJust $ find ((n==).fst) $ pokeballs atk)
                then return $ PKMN n
                else invalid "pokemon"
              else invalid "option"

            "info"    -> 
              case y of
                "yo"      -> info atk
                "rival"   -> info def
                otherwise -> invalid "info"
            otherwise -> invalid "option"
      otherwise -> invalid "option"
  where
    info trainer = do 
      putStrLn $ show.species.snd.getActive $ trainer
      turn atk def
    help trainer = (show.snd.getActive $ trainer) ++ "PokeParty\n" ++ pokeList atk