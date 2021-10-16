module Types.Mazo
    ( Mazo
    , Eleccion (..)
    , desdeMano
    , puedePicar
    , aplanar
    , reconstruir
    , robar
    , juegaLambda
    , elegir
    , getCard
    )
     where

{-|
Module      : Mazo
Description : Modulo que contiene la descripcion del tipo de dato Mazp
Maintainer  : 15-11139@usb.ve
Stability   : experimental
Portability : POSIX
-}


import Types.Carta ( Carta )
import Types.Mano
    ( Mano(..), vacia, valor, separar, (+++), isVacio )

-----------------
-- Tipos de Dato
-----------------

-- | Un mazo sera un arbol binario balanceado por altura.
data Mazo = Vacio | Mitad Carta Mazo Mazo

-- | Representa cual mitad del mazo selecciona el jugador
data Eleccion = Izquierdo | Derecho deriving (Show,Read)

--------------------------------
-- Constructores Inteligentes
--------------------------------

-- | Produce un mazo de tal forma que su recorrido inorder forme la
-- mano usada para construirlo
desdeMano :: Mano -> Mazo
desdeMano mano 
    | isVacio mano = Vacio
    | otherwise    = Mitad c (desdeMano l) (desdeMano r)
    where
        (l,c,r) = separar mano

elegir :: String -> Maybe Eleccion
elegir s
    | s `elem` ["l","left","i","izquierda"] = Just Izquierdo
    | s `elem` ["r","right","d","derecha"]  = Just Derecho
    | otherwise                             = Nothing         

--------------
-- Accesores
--------------

-- | Devuelve True si no es Vacio y ninguno de sus hijos es Vacio
puedePicar :: Mazo -> Bool
puedePicar (Mitad _ Mitad {} Mitad {}) = True
puedePicar _                           = False


getLeft :: Mazo -> Mazo
getLeft (Mitad _ l@(Mitad c _ _) _ ) = l

getRight :: Mazo -> Mazo
getRight (Mitad _  _ r@(Mitad c _ _)) = r

getCard :: Mazo -> Carta
getCard (Mitad c _ _) = c

--------------------------------
-- Funciones de Modificacion
--------------------------------

-- | Inorder traversal del Mazo
aplanar :: Mazo -> Mano
aplanar Vacio         = vacia 
aplanar (Mitad c l r) = Mano [c] +++ aplanar l +++ aplanar r

-- | Recibe el mazo original y una mano con las cartas jugadas en la ronda, produce
-- un mazo el cual tiene todas las cartes menos las que se jugaron en la ronda.
reconstruir :: Mazo -> Mano -> Mazo
reconstruir mazo (Mano m) = desdeMano $ Mano filtrado
    where
        (Mano lista) = aplanar mazo
        filtrado     = filter (`notElem` m) lista 
 
-- | Dado el mazo, la mano de un jugador, y una eleccion, retorna la mano y 
-- el mazo actualizados dependiendo de la eleccion, si no se puede picar el mazo
-- retorna `Nothing`.
robar :: Mazo -> Mano -> Eleccion -> Maybe (Mazo,Mano)
robar mazo mano eleccion 
    | puedePicar mazo = Just (mazo', Mano [c] +++ mano)
    | otherwise       = Nothing 
    where
        mazo' = case eleccion of
            Izquierdo -> getLeft mazo
            Derecho   -> getRight mazo
        c = getCard mazo'

-- | Recibe el mazo actual, la mano del dealer
-- y devuelve la Mano resultante tras robar hasta superar un valor de 16.
-- retorna `Nothing` si no quedan cartas que sacar.
juegaLambda :: Mazo -> Mano -> Maybe Mano
-- Uso de the Maybe Monad
juegaLambda mazo mano = do
    -- Trata de bindear el resultado de robar
    (mazo', mano') <- robar mazo mano Izquierdo
    -- si el valor es mayor a 16
    if valor mano' >= 16
        -- entonces encapusla mano' en un Just
        then return mano'
        -- sino, sigue robando (No hay que encapsular en Just
        -- puesto que juega lambda ya retorna Maybe a).
        else juegaLambda mazo' mano'