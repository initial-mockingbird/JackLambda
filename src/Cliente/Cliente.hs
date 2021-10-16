
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE RecordWildCards #-}
module Cliente.Cliente where
{-|
Module      : Cliente
Description : SIN USAR
Maintainer  : 15-11139@usb.ve
Stability   : experimental
Portability : POSIX

Trabajar Con el Stack de monadas: State, Cont e IO para implementar la segunda parte
del proyecto termina siendo un trabajo muy largo para terminarlo hoy :(

Asi que lo dejo in the trusting hands de la version que originalmente llegue hacer cuando
hice este proyecto: un menu recursivo.

-}

import Types.Mazo
import Types.Mano
import Types.Jugador
import Control.Monad.State.Strict
import System.Random
import Control.Monad.Cont

import Data.Maybe 
-----------------
-- Tipos de Dato
-----------------

data GameState = GS 
    { juegosJugados   :: Int     
    , victoriasLambda :: Int
    , nombre          :: String 
    , generador       :: StdGen -- ^ El generador para barajar las cartas
    , dinero          :: Int
    , objetivo        :: Int    -- ^ Una vez se supere, se termina el juego 
    , apuesta         :: Int    -- ^ Apuesta actual, $apuesta \leq dinero$
    } deriving (Show, Read)

data RoundState = RS 
    { manoLambda     :: Mano
    , manoPlayer     :: Mano
    , eleccion       :: Eleccion 
    , ganador_ronda  :: Maybe Jugador
    , mazoTurno      :: Mazo
    , nombre'        :: String
    , jugadas        :: Mano
    }

mazoInicial :: StdGen -> Mazo
mazoInicial gen = desdeMano $ barajar gen baraja



type Aux = ContT RoundState (StateT RoundState IO) ()

a :: Aux
a = do
    rs <- get 
    let (lambda,m') = fmap desdeMano $ inicialLambda $ aplanar $ mazoTurno rs
    
    matchResult <-  callCC $ \exit -> do
        when (blackjack lambda) $
            exit $ liftIO $ 
                putStrLn (show (nombre' rs) ++ ", he sacado blackjack. Yo Gano") 
                >> return rs{ganador_ronda = Just Dealer}

        let (Mano d) = manoLambda rs
        liftIO $ putStrLn ("Esta es mi primera carta: " ++ show (head d))



        return (return rs :: IO RoundState)

    --put matchResult
    return ()

b :: Mano -> RoundState -> (IO RoundState -> Aux) -> ContT RoundState (StateT RoundState IO) (IO RoundState)
b  lambda rs@(RS {..}) exit = do
    when (blackjack lambda) $
            exit $ liftIO $ 
                putStrLn (show nombre' ++ ", he sacado blackjack. Yo Gano") 
                >> return rs{ganador_ronda = Just Dealer}

    let (Mano d) = manoLambda
    liftIO $ putStrLn ("Esta es mi primera carta: " ++ show (head d))


    return $ return rs


robarCarta :: RoundState -> (IO RoundState -> Aux) -> ContT RoundState (StateT RoundState IO) (IO RoundState)
robarCarta rs@(RS {..}) exit = do
    
    let c1 = Mano [getCard mazoTurno]
    liftIO $ putStr (nombre' ++ ", robaras de la izquierda o de la derecha? (l/r): " )
    eleccionIO <- liftIO (elegir <$> readLn)

    when (isNothing eleccionIO) $
        exit $ liftIO $ do 
            putStrLn "Mal input, intente nuevamente" 
            return rs

    let Just eleccion = eleccionIO 
    let robo          = robar mazoTurno c1 eleccion
    let rearmado      = reconstruir mazoTurno jugadas
    when (isNothing robo) $
        exit $ liftIO $
            putStrLn (nombre' ++ ", tu mano es un blackjack!")
            >> return rs{ganador_ronda = Just Player }

    let Just (mazo',mano') = if isNothing robo then robar rearmado c1 eleccion else robo



    when (blackjack mano') $
        exit $ liftIO $
            putStrLn (nombre' ++ ", tu mano es un blackjack!")
            >> return rs{ganador_ronda = Just Player }

    liftIO $ putStrLn (nombre' ++ ", tu mano es: " ++ show mano')

    
    return $ return rs
    


jugarRonda :: StateT RoundState IO ()
jugarRonda = do 
    rs <- get 
    let (lambda,m') = fmap desdeMano $ inicialLambda $ aplanar $ mazoTurno rs
    liftIO $ putStrLn $
        nombre' rs ++ ","

    return ()