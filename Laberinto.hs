module Laberinto where

{-
    Laberinto con la información que el sabio
    conoce.
-}
data Laberinto =
    Laberinto Trifurcacion Tesoro

{-
    Trifurcación para un laberinto.
-}
data Trifurcacion =
    Trifurcacion { 
        tri_derecha :: Maybe Laberinto, -- Si se gira a la derecha
        tri_izquierda :: Maybe Laberinto, -- Si se gira a la izquierda
        tri_recto :: Maybe Laberinto  -- Si se sigue recto
    }

{-
    Tesoro a encontrar en un laberinto
-}
data Tesoro =
    Tesoro {
        tes_descripcion :: String, -- Descripción del tesoro
        tes_recto :: Maybe Laberinto -- Si se ignora el tesoro y se sigue recto
    }