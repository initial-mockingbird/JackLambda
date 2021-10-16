module Types.Jugador  (Jugador (..)) where
{-|
Module      : Jugador
Description : Modulo que contiene la descripcion del tipo de dato Jugador
Maintainer  : 15-11139@usb.ve
Stability   : experimental
Portability : POSIX

Representa a cualquiera de los dos jugadores de la partida

-}

-----------------
-- Tipos de Dato
-----------------

data Jugador = Dealer | Player deriving(Show,Read) -- Necesitaremos Pasar de Jugador a String y de String a Jugador.