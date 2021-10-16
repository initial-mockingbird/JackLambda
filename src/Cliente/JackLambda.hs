{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cliente.JackLambda (menuCarga) where

import System.Random 
import Types.Mazo
import Types.Mano
import Types.Jugador
import System.IO
import Control.Monad
import Data.Bifunctor

data GameState = GS {
  juegosJugados   :: Int,
  victoriasLambda :: Int,
  nombre          :: String,
  generador       :: StdGen,
  dinero          :: Int,
  objetivo        :: Int,
  apuesta         :: Int
  } deriving (Show, Read)

getInt :: IO Int
getInt = do
  entero <- getLine
  return (read entero :: Int )
  

menuCarga :: IO ()
menuCarga = do
  -- Lazy IO is not often desirable asi que vamos a flushear el stdout cada vez que pase algo
  hSetBuffering stdout NoBuffering
  cargar <-   putStr "Desea Cargar una partida? (y/n): " >> getLine 
  case cargar of
    "y" -> do
      partida <- putStrLn "Indique cual partida desea reanudar: " >> getLine
      cargarPartida partida
    "n" -> crearPartida
    _   -> putStrLn "Indique una opcion valida." >> menuCarga  
  return ()

crearPartida :: IO ()
crearPartida = do
  clear
  nombre <- getNombre
  dineroInicial <- getDinero
  dineroGanar <- getDineroGanar dineroInicial
  dineroRonda <- getDineroRonda dineroInicial
  juegoNuevo nombre dineroInicial dineroGanar dineroRonda
  return ()

getNombre :: IO String
getNombre = do
  nombre <- putStr "Indique su nombre: " 
  getLine 
  
getDinero :: IO Int
getDinero = do
  dinero <- putStr "Indique la cantidad de dinero con la que se debe empezar: " 
  getInt

getDineroGanar :: Int -> IO Int
getDineroGanar dineroInicial = do
  dineroGanar <- putStr "Indique la cantidad de dinero con la que se gana: " >> getInt
  if dineroGanar <= dineroInicial then putStrLn "La cantidad para ganar debe ser estrictamente mayor que la que se tiene, intente nuevamente." >>
    getDineroGanar dineroInicial else return dineroGanar 

getDineroRonda :: Int -> IO Int
getDineroRonda dineroInicial = do
  dineroRonda <- putStr "Indique la cantidad de dinero por apuesta: " >> getInt
  if  dineroRonda > dineroInicial then putStrLn "La apuesta  debe ser menor o igual que la que se tiene, intente nuevamente." >>
    getDineroRonda dineroInicial else return dineroRonda
  
juegoNuevo :: String -> Int -> Int -> Int -> IO ()
juegoNuevo nombre dinero objetivo apuesta = do
  clear 
  gen <- getStdGen 
  let juegoActual = GS { 
                    juegosJugados   = 0,
                    victoriasLambda = 0,
                    nombre          = nombre,
                    generador       = gen,
                    dinero          = dinero,
                    objetivo        = objetivo,
                    apuesta         = apuesta
                    }
  menuJuego juegoActual
  return ()

menuJuego :: GameState -> IO ()
menuJuego datos = do
  clear 
  putStrLn "Jugar Ronda (1)"
  putStrLn "Guardar Partida (2)"
  putStrLn "Cargar Partida (3)"
  putStrLn $ "Modificar apuesta: <apuesta actual: " ++ (show . apuesta) datos ++"> (4)"
  opcion <- putStr "Opcion: " >> getLine
  case opcion of
    "1" -> jugarRonda datos
    "2" -> guardarPartida datos
    "3" -> cargarPartida ""
    "4" -> modificarApuesta datos 
    _   -> putStrLn "Escoja una opcion valida" >> menuJuego datos
  return ()


modificarApuesta :: GameState -> IO ()
modificarApuesta gs = do
    putStr "Ingrese la nueva apuesta: "
    apuesta' <- read <$> getLine 
    menuJuego gs{apuesta=apuesta'}

jugarRonda :: GameState -> IO ()
jugarRonda datos@(GS{..}) = do
  clear 
  -- Dinero de la ronda
  let dinero' = dinero - apuesta 
  -- Nuevo estado con dinero:
  let datos' = GS{dinero = dinero', ..}
  -- Se baraja el mazo inicial
  let m0 = barajar generador baraja
  -- Se le dan 2 cartas a jack, y se les descuenta del mazo
  let (manoLambda,m1) = inicialLambda m0
  -- Verificacion de blackJack por parte de jack
  when (blackjack manoLambda) (ganaLambdaBlackJack datos')
  -- Se inicializa la mano del jugador
  let (manoPlayerInicial,m2) = bimap (Mano . (:[]) . getCard . desdeMano) desdeMano (m1,m1)
  putStrLn $ nombre  ++ ", Esta es mi primera carta: " ++ take 2 (show manoLambda)
  -- Seleccion de mazo.
  putStr  $ nombre ++ " robaras de la Izquierda o de la Derecha (Izquierdo/Derecho):  "
  choice <-  getLine
  -- Completamos la mano del jugador.
  -- Notemos que dado que esta es la mano inicial, NUNCA vamos a obtener un
  -- non exhaustive pattern.
  let Just (mazo,manoPlayer) = robar m2 manoPlayerInicial (read choice :: Eleccion)
  -- Print de la mano inicial del jugador. Y verificacion de blackjack
  if blackjack manoPlayer then ganaPlayerBlackJack datos'  else putStrLn (showMano manoPlayer Player nombre ) >>
    menuRonda datos' mazo manoPlayer manoLambda 
  return ()
  
menuRonda :: GameState -> Mazo -> Mano -> Mano -> IO ()
menuRonda datos mazo manoPlayer manoLambda  = do
  clear
  let puedeDouble = (<) 0 $ ((-) <$> dinero <*> apuesta) datos
  let puedeSurrender = cantidadCartas manoPlayer == 2
  putStrLn "Seleccione una opcion:"
  putStrLn "Hit (h)"
  putStrLn "Stand (s)"
  if puedeDouble then putStrLn "Double Down (d)" else pure ()
  if puedeSurrender then putStrLn "Surrender (ss)" else pure () 
  opcion <- putStr "Opcion: " >> getLine
  case (opcion, puedeDouble, puedeSurrender) of
    ("h", _, _)  -> hit datos mazo manoPlayer manoLambda 
    ("s",_,_)    -> stand datos mazo manoPlayer manoLambda 
    ("d",True,_) -> doubleDown datos mazo manoPlayer  manoLambda 
    ("ss",_,_)   -> surrender datos
    (_,_,_)      -> putStrLn "Indique una opcion valida" >> menuRonda datos mazo manoPlayer manoLambda
  
  return ()
  
hit :: GameState -> Mazo -> Mano -> Mano -> IO ()
hit datos mazo manoPlayer manoLambda = do
  let dividir = puedePicar mazo
  -- Si no se puede dividir el mazo, entonces se reconstruye con las cartas faltantes.
  let mazo' = if dividir then mazo else (reconstruir . reconstruir mazo) manoPlayer manoLambda
  eleccion <- putStr(nombre datos ++ ", robaras de izquierda o de derecha? (Izquierdo/Derecho): ") >>  getLine
  -- Notemos que si el mazo se puede dividir entonces hay al menos 1 carta en el mazo izquierdo o derecho
  -- de manera similar, al reconstruir el mazo, siempre se va a poder dividir en 2 mitades
  -- por lo tanto, podemos hacer el siguiente pattern matching sin obtener non-exhaustive pattern exception.
  let Just (mazo'',manoPlayer') = robar mazo' manoPlayer (read eleccion :: Eleccion )
  putStrLn (showMano manoPlayer' Player (nombre datos))
  if busted manoPlayer' then ganaLambda datos manoPlayer' manoLambda else menuRonda datos mazo'' manoPlayer' manoLambda
  return ()

stand ::  GameState -> Mazo -> Mano -> Mano -> IO ()
stand datos mazo manoPlayer manoLambda = do
  putStrLn "Es mi turno ahora." >> turnoLambda datos mazo manoPlayer manoLambda

doubleDown ::  GameState -> Mazo -> Mano -> Mano -> IO ()
doubleDown datos mazo manoPlayer manoLambda = do
  let datos' = datos {dinero = ((-) <$> dinero <*> apuesta) datos, apuesta = (*2) $ apuesta datos }
  (mazo', manoPlayer') <-  drawingPhasePlayer datos mazo manoPlayer manoLambda
  putStrLn ( showMano manoPlayer' Player  (nombre datos'))
  if busted manoPlayer' then ganaLambda datos manoPlayer' manoLambda else stand datos' mazo' manoPlayer' manoLambda
  return ()


surrender ::  GameState -> IO ()
surrender datos = do
  let datos' = datos {dinero = dinero datos + (apuesta datos `div` 2) }
  if dinero datos >= apuesta datos then menuJuego datos' else putStrLn $ nombre datos' ++  " no te queda dinero. Este es el fin para ti."
  return ()    

drawingPhasePlayer ::  GameState -> Mazo -> Mano -> Mano -> IO (Mazo,Mano)
drawingPhasePlayer datos mazo manoPlayer manoLambda = do
  let dividir = puedePicar mazo
  -- Si no se puede dividir el mazo, entonces se reconstruye con las cartas faltantes.
  let mazo' = if dividir then mazo else (reconstruir . reconstruir mazo) manoPlayer manoLambda
  eleccion  <- putStr(nombre datos ++ ", robaras de izquierda o de derecha? (Izquierdo/Derecho): ") >>  getLine
  -- Notemos que si el mazo se puede dividir entonces hay al menos 1 carta en el mazo izquierdo o derecho
  -- de manera similar, al reconstruir el mazo, siempre se va a poder dividir en 2 mitades
  -- por lo tanto, podemos hacer el siguiente pattern matching sin obtener non-exhaustive pattern exception.
  let Just (mazo'',manoPlayer') = robar mazo' manoPlayer (read eleccion :: Eleccion )
  return (mazo'',manoPlayer')


turnoLambda :: GameState -> Mazo -> Mano -> Mano -> IO ()
turnoLambda datos mazo manoPlayer manoLambda = do
  let Just manoLambda' = juegaLambda mazo manoPlayer
  let winner = ganador manoLambda' manoPlayer 
  case winner of
    Player -> ganaPlayer datos 
    Dealer -> ganaLambda datos manoPlayer manoLambda'
  return ()


ganaLambdaBlackJack :: GameState -> IO ()
ganaLambdaBlackJack datos = do
  putStrLn $ nombre datos ++ ", he sacado blackJack, yo gano."
  let datos' = datos {juegosJugados = juegosJugados datos +1,
                  victoriasLambda = victoriasLambda datos +1}

  if dinero datos' < apuesta datos' then putStrLn ( (show . nombre) datos' ++ ", no te queda dinero. Es el fin del juego para ti.")
    else menuJuego datos'
  return ()

ganaPlayerBlackJack :: GameState -> IO ()
ganaPlayerBlackJack datos = do
  putStrLn ( nombre datos ++ ", tu mano es un blackJack, tu ganas.")
  let datos' = datos {juegosJugados = juegosJugados datos +1,
                  dinero = dinero datos + 2 * apuesta datos
                 }
  if dinero datos' >= objetivo datos' then
    putStrLn ( "Felicidades " ++ (show . nombre) datos' ++ ", me has derrotado. Es el fin del juego para mi.")
    else menuJuego datos'
  return ()
   

showMano :: Mano -> Jugador -> String ->  String
showMano mano jugador nombre = case jugador of
  Player -> nombre ++ " tu mano es: " ++ show mano ++ "\n" ++
                               "Suma: " ++ (show . valor) mano  ++ (if busted mano then " Perdiste" else "")
  _      -> "Mi mano es: " ++ show mano ++ "\n" ++
                               "Suma: " ++ (show . valor) mano 


ganaLambda :: GameState -> Mano -> Mano -> IO ()
ganaLambda datos manoPlayer manoLambda = do
  let datos' = datos {juegosJugados = juegosJugados datos +1,
                  victoriasLambda = victoriasLambda datos +1}
  if valor manoPlayer == valor manoLambda then
    putStrLn "Empatamos, asi que gano yo" else putStrLn "Yo gano."

  if dinero datos' < apuesta datos' then putStrLn ( (show . nombre) datos' ++ ", no te queda dinero. Es el fin del juego para ti.")
    else menuJuego datos'
  return ()

ganaPlayer :: GameState -> IO ()
ganaPlayer datos = do
  putStrLn "Tu ganas."
  let datos' = datos {juegosJugados = juegosJugados datos +1,
                  dinero = dinero datos + 2 * apuesta datos
                 }
  if dinero datos' > objetivo datos' then
    putStrLn ( "Felicidades " ++ (show . nombre) datos' ++ ", me has derrotado. Es el fin del juego para mi.")
    else menuJuego datos'
  return ()


clear :: IO ()
clear = putStrLn ""


guardarPartida :: GameState -> IO ()
guardarPartida datos = do
    writeFile "./Partida.txt" $ show datos

cargarPartida :: String -> IO ()
cargarPartida nombre = do
    estado <- read <$> readFile nombre
    menuJuego estado
  







