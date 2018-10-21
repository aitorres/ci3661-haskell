{-|
Module      : Laberinto
Description : Modulo con las funciones y tipos de datos que permiten la interacción y construcción de un laberinto.
License     : MIT
Maintainer  : gustavoaca1997@gmail.com
Modulo con las funciones y tipos de datos que permiten la interacción y construcción de un laberinto.
-}
module Laberinto where
--------------------------------------------------------------------------------------------
-- * Tipos de datos

-- | Laberinto con la información que el sabio
-- conoce.
data Laberinto =
    Laberinto {
        -- | Trifurcación asociada al laberinto.
        trifurcacionLaberinto :: Trifurcacion,

        -- | Tesoro asociado al laberinto
        tesoroLaberinto :: Maybe Tesoro
    }
    deriving Show
{-|
    Trifurcación para un laberinto.
-}
data Trifurcacion =
    Trifurcacion { 
        derechaTrifurcacion :: Maybe Laberinto, -- ^ Si se gira a la derecha
        izquierdaTrifurcacion :: Maybe Laberinto, -- ^ Si se gira a la izquierda
        rectoTrifurcacion :: Maybe Laberinto  -- ^ Si se sigue recto
    }
    deriving Show

{-|
    Tesoro a encontrar en un laberinto
-}
data Tesoro =
    Tesoro {
        descripcionTesoro :: String, -- ^ Descripción del tesoro
        rectoTesoro :: Maybe Laberinto -- ^ Si se ignora el tesoro y se sigue recto
    }
    deriving Show
--------------------------------------------------------------------------------------------

-- * Funciones de Construcción

{- | Función que retorna un camino sin salida -}
caminoDefault :: Trifurcacion
caminoDefault = Trifurcacion { 
        derechaTrifurcacion=Nothing, 
        izquierdaTrifurcacion=Nothing,
        rectoTrifurcacion=Nothing
    }

{-| Función que retorna un laberinto default con camino sin salida y sin tesoro -}
laberintoDefault :: Laberinto
laberintoDefault = Laberinto {
    trifurcacionLaberinto = caminoDefault,
    tesoroLaberinto = Nothing
}

{- |Función que recibe un String con la descripción de un tesoro y un laberinto
indicando qué encontrarán si pasan por alto el tesoro, y retorna el Tesoro. -}
crearTesoro :: String -> Maybe Laberinto -> Tesoro
crearTesoro descripcion laberinto = Tesoro {
    descripcionTesoro=descripcion,
    rectoTesoro=laberinto
}

{- |Función que recibe una Trifurcacion, un laberinto y un indicador de cuál camino
los relaciona (izquierda, derecha, recto), y retorna una Trifurcacion en la que se
indique que dicho camino conduce a dicho laberinto. -}
unirLaberinto :: Trifurcacion -> Maybe Laberinto -> String -> Trifurcacion
unirLaberinto trifurcacion laberinto camino =
    case camino of
        "izquierda" -> trifurcacion { izquierdaTrifurcacion = laberinto }
        "derecha" -> trifurcacion { derechaTrifurcacion = laberinto }
        "recto" -> trifurcacion { rectoTrifurcacion = laberinto }

{-| Función que construye un laberinto a partir de una ruta -}
construirLaberinto :: [String] -> Maybe Laberinto
construirLaberinto [] = Just $ laberintoDefault
construirLaberinto (c:cs) =
    case c of
        "izquierda" -> Just laberintoDefault { 
            trifurcacionLaberinto = caminoDefault { izquierdaTrifurcacion = construirLaberinto cs}
        }

        "derecha" -> Just laberintoDefault { 
            trifurcacionLaberinto = caminoDefault { derechaTrifurcacion = construirLaberinto cs}
        }

        "recto" -> Just laberintoDefault { 
            trifurcacionLaberinto = caminoDefault { rectoTrifurcacion = construirLaberinto cs}
        }

--------------------------------------------------------------------------------------------

-- * Funciones de Acceso

{- |Función que recibe un laberinto y una ruta y retorna el laberinto que comienza en el
punto al que conduce esa ruta -}
recorrer :: Maybe Laberinto -> [String] -> Maybe Laberinto
recorrer Nothing _ = Nothing
recorrer lab [] = lab
recorrer lab@(Just laberinto) (c:cs) = recorrer caminoEscogido' cs
        where 
            trifurcacion = trifurcacionLaberinto laberinto -- trifurcación del laberinto
            
            -- Camino escogido por el usuario
            caminoEscogido = case c of
                "izquierda" -> izquierdaTrifurcacion trifurcacion
                "derecha" -> derechaTrifurcacion trifurcacion
                "recto" -> rectoTrifurcacion trifurcacion

            -- En caso de encontrarse con una pared, ignorar el paso actual
            caminoEscogido' = case caminoEscogido of
                Nothing -> lab -- ignorar paso
                l -> l

{- |Función que recibe un laberinto y retorna el laberinto que comienza al voltear a la
izquierda -}
voltearIquierda :: Maybe Laberinto -> Maybe Laberinto
voltearIquierda lab = recorrer lab ["izquierda"]

{- |Función que recibe un laberinto y retorna el laberinto que comienza al voltear a la
derecha -}
voltearDerecha :: Maybe Laberinto -> Maybe Laberinto
voltearDerecha lab = recorrer lab ["derecha"]

{- |Función que recibe un laberinto y retorna el laberinto que comienza al seguir recto -}
seguirRecto :: Maybe Laberinto -> Maybe Laberinto
seguirRecto lab = recorrer lab ["recto"]