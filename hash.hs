module Main (main) where
import Data.List.Split (splitOn)
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Data.Tuple (swap)
import qualified Data.IntMap as IMap
import System.IO
import System.Environment
import HashPoke

main = do
  args <- getArgs
  putStrLn " ======================================================================================="
  putStrLn "|| atacar = FIGHT, cambiar = PKMN, info = INFO, rendirse = RUN, yo = ME, rival = RIVAL ||"
  putStrLn " ======================================================================================="
  putStrLn "Let's PokeBattle!\n"
  --print args
  if length args /= 4 then putStrLn "Usage: pokemons.csv moves.csv trainer1.csv trainer2.csv"
  else
    -- Inicializando listas desde los archivos a usar en el programa entero
    do
    speciesFile <- readFile $ args !! 0
    movesFile   <- readFile $ args !! 1
    train1File  <- readFile $ args !! 2
    train2File  <- readFile $ args !! 3
    let species  = speciesParse $ linesParse speciesFile
    let moves    = movesParse   $ linesParse movesFile
    let trainer1 = trainerParse (linesParse train1File) species moves
    let trainer2 = trainerParse (linesParse train2File) species moves
    winner <- battle trainer1 trainer2

    if winner /= DRAW then
      do putStrLn $ (show winner) ++ " won!"
    else
      do putStrLn "Draw!"


linesParse :: String -> [[String]]
linesParse s = map (splitOn (",")) $ filter ("" /=) $ lines s

speciesParse :: [[String]] -> IMap.IntMap Species
speciesParse list = IMap.fromList $ map (\x -> (no x,x)) $ map getEvolutions $ listSpecies
  where
    listSpecies = map species list
    species [sNo, sName, sPT1, sPT2, sHP, sAtk, sDef, sSpAtk, sSpDef, sSpd, sPreE, sCr] = 
      Species 
        { no       = read sNo :: Int
        , name     = sName
        , pokeType = (read sPT1 :: Type) 
                      : (if sPT2 /= "" then (read sPT2 :: Type) : [] else [])
        , base     = 
            Stats 
              { hp        = read sHP    :: Int
              , attack    = read sAtk   :: Int
              , defense   = read sDef   :: Int
              , spAttack  = read sSpAtk :: Int
              , spDefense = read sSpDef :: Int
              , speed     = read sSpd   :: Int
              }
        , preEvolution = 
            if sPreE /= "" 
            then Just Evolution {eNo = read sPreE :: Int, criterio = sCr} 
            else Nothing
        , evolutions   = []
        }
    getEvolutions :: Species -> Species
    getEvolutions x = 
      x { evolutions = 
            let pre = fromJust.preEvolution
            in map (\y -> Evolution { eNo = no y, criterio = criterio $ pre y}) 
                $ filter ((no x==).eNo.pre) 
                  $ filter (isJust.preEvolution)
                    $ listSpecies
        }

movesParse :: [[String]] -> [Move]
movesParse = map (\p -> moves p)
  where
    moves [mName, mType, mPhy, mPP, mPWR] = 
      Move 
        { moveName = mName
        , moveType = read mType :: Type
        , physical = read mPhy  :: Bool
        , pp       = read mPP   :: Int
        , power    = read mPWR  :: Int
        }

trainerParse :: [[String]] -> IMap.IntMap Species -> [Move] -> Trainer
trainerParse x specs moves = Trainer {active = 0, pokeballs = map monster x}
  where
    monster [pSpec, pNick, pLVL, pAtk1, pAtk2, pAtk3, pAtk4] = 
      mons { presHP = maxHP mons
           , stats  = 
              Stats 
                { hp        = maxHP mons
                , attack    = stat  mons attack
                , defense   = stat  mons defense
                , spAttack  = stat  mons spAttack
                , spDefense = stat  mons spDefense
                , speed     = stat  mons speed
                }
           }
      where
        mons = 
          Monster 
            { species  = pokeSpecies
            , nickname = if pNick == "" then name pokeSpecies else pNick
            , lvl      = read pLVL :: Int
            , presHP   = 0 
            , moves    = getMoves $ filter ("" /=) [pAtk1, pAtk2, pAtk3, pAtk4]
            , stats    = base pokeSpecies
            , iv       = perfIV
            , ev       = perfEV
            }
          where
            pokeSpecies = fromJust $ IMap.lookup (read pSpec :: Int) specs
            getMoves = map (\n -> MonsterMove {monMove = move n, monPP = pp $ move n})
              where
                move n = fromJust $ find (\m -> moveName m == n) moves
            perfIV = Stats 31  31  31  31  31  31 
            perfEV = Stats 255 255 255 255 255 255

data Result 
  = Trainer1
  | Trainer2 
  | DRAW 
  deriving (Show, Eq)

data Option
  = FIGHT Int 
  | PKMN Int 
  | RUN 
  deriving (Eq)

getActive :: Trainer -> Monster
getActive trainer = pokeballs trainer !! active trainer

fainted :: Monster -> Bool
fainted poke = presHP poke <= 0

defeated :: Trainer -> Bool
defeated trainer = all fainted $ pokeballs trainer

attacks :: Int -> Trainer -> Trainer -> (Trainer,Trainer)
attacks movN atk def = 
  ( atk { pokeballs = 
    map (\x -> 
      if x == actA 
      then x { moves = 
        map (\y -> 
          if y == mov 
          then y { monPP = (monPP mov - 1) } 
          else y) 
        $ moves actA } 
      else x) 
    $ pokeballs atk }

  , def { pokeballs = 
    map (\x -> 
      if x == actD 
      then x { presHP = 
        if (presHP actD) < dmg 
        then 0 
        else (presHP actD - dmg)} 
      else x) 
    $ pokeballs def }
  )
  where
    mov  = (moves actA) !! movN
    actA = getActive atk
    actD = getActive def
    dmg  = damage (monMove mov) (getActive atk) (getActive def)

pkmn :: Trainer -> Option -> Trainer
pkmn trainer (PKMN x) = trainer {active = x}
pkmn trainer _      = trainer

checkSpeed :: (Trainer, Int) -> (Trainer, Int) -> (Trainer,Trainer)
checkSpeed (left, l) (right, r)
  | getSpeed left >= getSpeed right = 
    let 
      (nLeft, nRight) = attacks l left right
    in 
      if fainted $ getActive nRight then (nLeft, nRight)
      else swap $ attacks r nRight nLeft
  | otherwise                       =
    let 
      (nRight, nLeft) = attacks r right left
    in 
      if fainted $ getActive nLeft then (nLeft, nRight)
      else attacks l nLeft nRight
  where
    getSpeed trainer = speed $ stats $ getActive trainer

battle :: Trainer -> Trainer -> IO Result
battle left right =
  do
  putStrLn $ "Trainer1's " ++ show left ++ "\n"
  putStrLn $ "Trainer2's " ++ show right ++ "\n"
  case (defeated left, defeated right) of
    (True, True) -> return DRAW
    (_, True)    -> return Trainer1
    (True, _)    -> return Trainer2
    otherwise    -> do
      putStrLn "What will you do Trainer1!?"
      -- Si el pokemon actual esta fainted, lo cambiamos a uno activo
      aLeft <- chooseActive left
      -- Pido un comando de accion
      comm1 <- turn aLeft right
      putStrLn "What will you do Trainer2!?"
      aRight <- chooseActive right
      comm2 <- turn aRight left
      -- En caso de cambio de pokemon
      let nLeft = pkmn aLeft comm1 
      let nRight = pkmn aRight comm2
      -- Posibles combinaciones de flujo de batalla
      case (comm1, comm2) of
        (RUN, RUN)         -> return DRAW
        (_, RUN)           -> return Trainer1
        (RUN, _)           -> return Trainer2
        (PKMN x, PKMN y)   -> battle nLeft nRight
        (PKMN x, FIGHT y)  -> uncurry battle $ swap $ attacks y nRight nLeft
        (FIGHT x, PKMN y)  -> uncurry battle $ attacks x nLeft nRight
        (FIGHT x, FIGHT y) -> uncurry battle $ checkSpeed (nLeft,x) (nRight,y)

chooseActive :: Trainer -> IO Trainer
chooseActive trainer
  | fainted $ getActive trainer = do
    putStrLn "Choose a new Pokemon to battle!"
    putStrLn pokeList
    input <- getLine
    let newP = (read input :: Int) - 1
    let nTrainer = trainer { active = if (0 <= newP) 
                                  && ((<) newP $ length $ pokeballs trainer) 
                                  && (not $ fainted $ (pokeballs trainer) !! newP) 
                              then newP 
                              else active trainer }
    putStrLn $ show nTrainer
    chooseActive nTrainer
  | otherwise                   = return trainer
  where
    pokeList = foldr format "" $ zip [1..] $ map tupleFormat $ pokeballs trainer
      where
      format (x,(n,l,h,t)) y = 
        (show x ++ " -> " ++ n ++ " LVL" ++ show l ++ " " 
         ++ show h ++ "/" ++ show t ++ "HP\n") ++ y
      tupleFormat x          = (nickname x, lvl x, presHP x,hp.stats $ x)
turn :: Trainer -> Trainer -> IO Option
turn atk def =
  do
  putStrLn "================================="
  putStrLn "| FIGHT N         -      PKMN N |"
  putStrLn "| INFO [ME|RIVAL] -  HELP - RUN |"
  putStrLn "=================================\n"
  input <- getLine
  let 
    (x:xs)  = splitOn " " input
    invalid = do 
      putStrLn "Invalid option...\n"
      turn atk def
   in if length (x:xs) == 1 then
         case x of
           "ayuda"    -> do
             putStrLn $ help atk
             turn atk def
           "rendirse" -> return RUN
           otherwise  -> invalid
       else if length (x:xs) == 2 then
         case x of
           "atacar"  -> let n = (read (head xs) :: Int) - 1
                         in if (0 <= n) && ((<) n $ length $ moves $ getActive atk)
                            then return $ FIGHT n
                            else invalid
           "cambiar" -> let n = (read (head xs) :: Int) - 1 
                         in if (n /= active atk) 
                                && (0 <= n) && ((<) n $ length $ pokeballs atk) 
                                && (not $ fainted $ (pokeballs atk) !! n)
                            then return $ PKMN n
                            else invalid
           "info"    -> 
             case (head xs) of
               "yo"      -> do 
                 putStrLn $ info atk
                 turn atk def
               "rival"   -> do 
                 putStrLn $ info def
                 turn atk def
               otherwise -> invalid
           otherwise -> invalid
      else invalid
  where
    info trainer = show $ pokeballs trainer
    help trainer = (show $ moves $ getActive trainer) 
                    ++ "\n" ++ (show $ zip [1..] $ pokeballs trainer)