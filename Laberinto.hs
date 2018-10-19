module Laberinto where

{-
    Laberinto con la información que el sabio
    conoce.
-}
data Laberinto =
    Laberinto Trifurcacion Tesoro
    deriving Show
{-
    Trifurcación para un laberinto.
-}
data Trifurcacion =
    Trifurcacion { 
        derechaTrifurcacion :: Maybe Laberinto, -- Si se gira a la derecha
        izquierdaTrifurcacion :: Maybe Laberinto, -- Si se gira a la izquierda
        rectoTrifurcacion :: Maybe Laberinto  -- Si se sigue recto
    }
    deriving Show

{-
    Tesoro a encontrar en un laberinto
-}
data Tesoro =
    Tesoro {
        descripcionTesoro :: String, -- Descripción del tesoro
        rectoTesoro :: Maybe Laberinto -- Si se ignora el tesoro y se sigue recto
    }
    deriving Show

{-
FUNCIONES DE CONSTRUCCION
-}

{- Una función que retorne un camino sin salida -}
caminoSinSalida :: Trifurcacion
caminoSinSalida = Trifurcacion { 
        derechaTrifurcacion=Nothing, 
        izquierdaTrifurcacion=Nothing,
        rectoTrifurcacion=Nothing
    }