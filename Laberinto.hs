module Laberinto where
--------------------------------------------------------------------------------------------
{-
    Laberinto con la información que el sabio
    conoce.
-}
data Laberinto =
    Laberinto {
        trifurcacionLaberinto :: Trifurcacion,
        tesoroLaberinto :: Tesoro
    }
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
--------------------------------------------------------------------------------------------

{-
FUNCIONES DE CONSTRUCCION
-}

{- Fnción que retorna un camino sin salida -}
caminoDefault :: Trifurcacion
caminoDefault = Trifurcacion { 
        derechaTrifurcacion=Nothing, 
        izquierdaTrifurcacion=Nothing,
        rectoTrifurcacion=Nothing
    }

{- Función que recibe un String con la descripción de un tesoro y un laberinto
indicando qué encontrarán si pasan por alto el tesoro, y retorna el Tesoro. -}
crearTesoro :: String -> Maybe Laberinto -> Tesoro
crearTesoro descripcion laberinto = Tesoro {
    descripcionTesoro=descripcion,
    rectoTesoro=laberinto
}

{- Función que recibe una Trifurcacion, un laberinto y un indicador de cuál camino
los relaciona (izquierda, derecha, recto), y retorna una Trifurcacion en la que se
indique que dicho camino conduce a dicho laberinto. -}
unirLaberinto :: Trifurcacion -> Maybe Laberinto -> String -> Trifurcacion
unirLaberinto trifurcacion laberinto camino =
    case camino of
        "izquierda" -> trifurcacion { izquierdaTrifurcacion = laberinto }
        "derecha" -> trifurcacion { derechaTrifurcacion = laberinto }
        "recto" -> trifurcacion { rectoTrifurcacion = laberinto }

--------------------------------------------------------------------------------------------

{-
FUNCIONES DE ACCESO
-}

{- Una función que reciba un laberinto y una ruta y retorne el laberinto que comienza en el
punto al que conduce esa ruta -}
recorrer :: Maybe Laberinto -> [String] -> Maybe Laberinto
recorrer Nothing _ = Nothing
recorrer laberinto [] = laberinto
recorrer (Just laberinto) (c:cs) = recorrer caminoEscogido cs
        where 
            trifurcacion = trifurcacionLaberinto laberinto -- trifurcación del laberinto
            caminoEscogido = case c of
                "izquierda" -> izquierdaTrifurcacion trifurcacion
                "derecha" -> derechaTrifurcacion trifurcacion
                "recto" -> rectoTrifurcacion trifurcacion