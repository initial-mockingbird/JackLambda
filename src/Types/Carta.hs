-- Trabajar con records puede ser verboso, esto decrementa
-- la verbosidad:
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_puns.html
{-# LANGUAGE NamedFieldPuns #-}
module Types.Carta
    -- Cosas que se exportar
    ( Palo  (..) -- Se exporta el tipo Palo  junto a TODOS sus constructores
    , Rango (..) -- Se exporta el tipo Rango junto a TODOS sus constructores
    , Carta (..) -- Se exporta el tipo Carta junto a TODOS sus constructores
    ) where
{-|
Module      : Carta
Description : Modulo que contiene la descripcion del tipo de dato Carta
Maintainer  : 15-11139@usb.ve
Stability   : experimental
Portability : POSIX

El tipo de datosCartadebe ser capaz de representar cualquier carta de la baraja francesa est ́andar de 52 cartas.Para ello debemos tener sendos tipos de datos para representar el palo y el rango de una carta:data Palo = Treboles | Diamantes | Picas | Corazonesdata Rango = N Int | Jack | Queen | King | AceUna vez teniendo esto, podemos definir el tipo de datos que represente una carta cualquiera:data Carta = Carta {rango :: Rango,palo  :: Palo}Adicionalmente a estas definiciones, se deben suministrar las siguientes funciones:Instancias:•Una instancia de la claseShow, de manera que reciba una carta y devuelva una cadena de caracteresque contenga una representaci ́on de la mismaentendiblepara el humano. Para ello, debe mostrar loss ́ımbolos asociados con el juego propiamente. Por ejemplo, las cartas:Card Ace CorazonesCard (N 10) Picasdeben presentarse en pantalla como:♡A♠10•Cualquier otra instancia que se estime necesaria para hacer posible la implementaci ́on.2

-}


-----------------
-- Tipos de Dato
-----------------

data Palo = Treboles | Diamantes | Picas | Corazones deriving (Enum, Eq)

data Rango = N Int | Jack | Queen | King | Ace deriving(Eq)

data Carta = Carta
    { rango :: Rango
    , palo  :: Palo 
    } deriving(Eq)

---------------
-- Instancias
---------------

-- Instancias de Show para poder
-- transformar a String.

instance Show Palo where
    show Treboles  = "T"
    show Diamantes = "D"
    show Picas     = "P"
    show Corazones = "C"

instance Show Rango where
    show (N n) = show n
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"

instance Show Carta where
    show Carta {rango,palo} = show palo ++ show rango

