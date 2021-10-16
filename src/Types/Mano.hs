-- Trabajar con records puede ser verboso, esto decrementa
-- la verbosidad:
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_puns.html
{-# LANGUAGE NamedFieldPuns #-}
module Types.Mano 
    -- Cosas que se exportar
    ( Mano(..)  -- Se exporta el tipo y el constructor
    , vacia
    , isVacio
    , baraja
    , cantidadCartas
    , valor
    , busted
    , blackjack
    , ganador
    , separar
    , barajar
    , (+++)
    , inicialLambda)
    where
{-|
Module      : Mano
Description : Modulo que contiene la descripcion del tipo de dato Mano
Maintainer  : 15-11139@usb.ve
Stability   : experimental
Portability : POSIX

Para controlar las cartas que tiene cada jugador en la mano
-}


import Types.Carta
    ( Carta(..), Rango(N, King, Queen, Jack, Ace), Palo(Treboles) )
    
import Types.Jugador ( Jugador(..) ) 
import System.Random ( Random(randomR), RandomGen(split), StdGen ) 
import Data.Bifunctor ( Bifunctor(bimap) )


-----------------
-- Tipos de Dato
-----------------

-- | Una mano es solo un conjunto de cartas
newtype Mano = Mano [Carta] 

--------------------------------
-- Constructores Inteligentes
--------------------------------

-- | Produce una `Mano` Vacia
vacia :: Mano
vacia = Mano []



-- | Produce  una `Mano` que  contenga  las  52  cartas  de  la  baraja  estandar  
-- francesa.  Esto  es,  una  de  cada combinacion palo-rango
baraja :: Mano
baraja = Mano cartas
    where
        -- Creamos una lista de todos los rangos posibles
        rangos = Ace : King : Queen : Jack : [N n | n <- [2..10]]
        -- Creamos una lista de todos los palos posibles
        palos  = [Treboles ..]
        -- Finalmente, todas las cartas posibles no es nada mas que
        -- el producto cartesiano de palos con rangos usando el constructor de 
        -- cartas
        cartas = map (\rango palo -> Carta {rango,palo}) rangos <*> palos

--------------
-- Accesores
--------------

-- | Determina si una mano esta vacia
isVacio :: Mano -> Bool
isVacio (Mano []) = True
isVacio _         = False

-- | Determina la cantidad de cartas en una mano
cantidadCartas :: Mano -> Int
cantidadCartas (Mano m) = length m

-- | Recibe una Mano y devuelve un entero con el valor de la misma.
valor :: Mano -> Int
valor (Mano m) 
    -- Si la suma contando el Ace como 11 no supera 21 entonces la devolvemos
    | suma_no_bust <= 21 = suma_no_bust
    -- Si si lo supera, devolvemos la misma suma considerando todos los Ace como 1
    | otherwise          = suma_bust
    where
        -- Convierte una carta a su valor entero
        valor' :: Bool -> Carta -> Int
        valor' b Carta{rango,palo} = case rango of
            Ace -> if b then 11 else 1
            N n -> n
            _   -> 10
        
        suma_no_bust = sum $ map (valor' True) m
        suma_bust    = sum $ map (valor' False) m

-- | Recibe unaManoy devuelveTruesi su valor excede los 21, yFalsede otra forma
busted :: Mano -> Bool
busted mano = valor mano > 21

-- | Recibe una Mano y devuelve `True` si la mano es un __blackjack__ 
-- y `False` de otra forma. 
blackjack :: Mano -> Bool
blackjack mano = valor mano == 21

-- | Recibe la Mano del `Dealer` de primero, la mano del `Player` de segundo, 
-- y devuelve el ganador, segun las reglas del juego antes descritas.
ganador :: Mano -> Mano -> Jugador
ganador dealer player 
    | busted player      = Dealer 
    | busted dealer      = Player
    | blackjack dealer   = Dealer 
    | valor_d >= valor_p = Dealer 
    | otherwise          = Player
    where
        valor_d = valor dealer
        valor_p = valor player

{- |
Recibe una `Mano` y la separa en una tupla `(l,c,r)` de la siguiente manera:

    * Si la mano es de longitud impar, `c` sera el elemento del medio de la lista, y `l`,
    `r` seran respectivamente las mitades izquierda y derecha

    * Si la mano es de longitud par, `l` sera la mitad izuiqerda, `c` sera el primer
    elemento de la mitad derecha y `r` sera el resto de la mitad derecha
-}
separar :: Mano -> (Mano, Carta, Mano)
separar (Mano m) = (Mano l,c,Mano r)
    where
        lm      = length m
        (l,c:r) = splitAt (lm `div` 2) m



--------------------------------
-- Funciones de Modificacion
--------------------------------

-- Operador infijo (+++) con precedencia 5 y que asocia a la derecha
infixr 5  +++

-- | Concatenacion de manos
(+++) :: Mano -> Mano -> Mano
(Mano m) +++ (Mano n) = Mano $ m ++ n

-- | Usada para barajar la mano al inicio de la ronda, la forma en que
-- se baraja es eligiendo una carta al azar y poniendola al inicio de una
-- mano acumuladora.
barajar :: StdGen -> Mano -> Mano
-- as patterns son tremendos para no tener que repetir (Mano m)
barajar gen mano@(Mano m) = barajar' (length m) gen mano

-- Funcion auxiliar que utiliza barajar que utiliza un entero
-- para no tener que calcular el length de la lista cada vez
barajar' :: Int -> StdGen -> Mano -> Mano 
barajar' upperBound gen (Mano []) = Mano []
barajar' upperBound gen (Mano [a]) = Mano [a]
barajar' upperBound gen (Mano m) 
    -- Anado el elemento en la i-esima posicion
    =   Mano [m !! index]
    -- lo concateno con el resultado de barajar el slice izquierdo
    +++ barajar' (index-1) g l_slice 
    -- y eso lo concateno con el resultado de barajar el slice derecho
    +++ barajar' (upperBound - index - 1) g' r_slice
    where
        -- Es bueno mencionar los tipos cuando se trabaja con Random
        index       :: Int
        gen', g, g' :: StdGen

        -- Elegimos un indice de forma aleatoria, y obtenemos un nuevo generador
        (index,gen') = randomR (0 :: Int,upperBound-1) gen
        -- Dividimos el generador en dos para poder usarlos en la parte izquierda
        -- y derecha
        (g,g')       = split gen'

        -- Realizamos la particion
        (l_slice',val:r_slice')  = splitAt index m
        
        -- Encapsulacion fastidiosa, debo averiguar si hay una forma de hacer eso
        -- de mejor forma
        l_slice = Mano l_slice'
        r_slice = Mano r_slice'

-- | Recibe la mano inicial barajada, y devuelve la mano de lambda
-- la cual consta de las dos primeras cartas del mazo, y el mazo actualizado
inicialLambda :: Mano -> (Mano,Mano)
-- map Mano ([xsy],[yzv]) = ([xsy],Mano [yzv])
-- pero: bimap Mano Mano  = (Mano [xsy],Mano [yzv])
inicialLambda (Mano m) = bimap Mano Mano $ splitAt 2 m

---------------
-- Instancias
---------------

-- Instancias de Show para poder
-- transformar a String.
instance Show Mano where
    -- foldr es reduce en lenguajes tipo python o js
    -- solo que es de derecha a izquierda en vez de izquierda a
    -- derecha
    show (Mano m) = foldr (\carta acc -> show carta ++ acc) "" m