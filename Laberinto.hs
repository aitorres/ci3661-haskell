{-|
Module      : Laberinto
Description : Módulo con las funciones y tipos de datos que permiten la construcción de un laberinto e interacciones con él.
License     : MIT
Maintainer  : gustavoaca1997@gmail.com, andresitorresm@gmail.com
Stability   : stable
Portability : POSIX

Módulo que incluye las funciones y tipos de datos que permiten la construcción de un laberinto,
así como la interacción con ellos a través de rutas, trifurcaciones y tesoros. Incluye también
funciones auxiliares utilizadas tanto de manera privada como en el cliente principal ("Main").
-}

module Laberinto where

--------------------------------------------------------------------------------------------
-- * Tipos de datos
--
-- $tiposDeDatos
-- Se definen distintos tipos de datos para el manejo de Laberintos ('Laberinto'), así como 
-- de sus caminos ('Trifurcacion'), los tesoros que pueden estar en el laberinto ('Tesoro') 
-- y las rutas que se pueden recorrer en los mismos ('Ruta').

-- | Lista de caminos a tomar
type Ruta = [String]

{-| Laberinto con la información que el Sabio
    conoce.
-}
data Laberinto =
    Laberinto {
        trifurcacionLaberinto :: Trifurcacion, -- ^ Trifurcación que indica los caminos del laberinto.
        tesoroLaberinto :: Maybe Tesoro  -- ^ Tesoro asociado al laberinto
    }
    deriving (Show, Read)

{-|
    Trifurcación que indica los caminos posibles en un 'Laberinto'.
-}
data Trifurcacion =
    Trifurcacion { 
        derechaTrifurcacion :: Maybe Laberinto, -- ^ Si se gira a la derecha
        izquierdaTrifurcacion :: Maybe Laberinto, -- ^ Si se gira a la izquierda
        rectoTrifurcacion :: Maybe Laberinto  -- ^ Si se sigue recto
    }
    deriving (Show, Read)

{-|
    Tesoro que puede ser ubicado dentro de un 'Laberinto'.
-}
data Tesoro =
    Tesoro {
        descripcionTesoro :: String, -- ^ Descripción del tesoro.
        rectoTesoro :: Maybe Laberinto -- ^ Laberinto al que se accede si se ignora el tesoro y se sigue recto.
    }
    deriving (Show, Read)

--------------------------------------------------------------------------------------------
-- * Funciones de Construcción
--
-- $construccion
-- Este apartado incluye funciones que permiten crear y construir instancias de los
-- distintos tipos de datos definidos, de modo que sea más sencillo trabajar con ellos.


{-| 
    Función que devuelve un camino ('Trifurcacion') sin salida. 
-}
caminoDefault :: Trifurcacion
caminoDefault = Trifurcacion { 
        derechaTrifurcacion=Nothing, 
        izquierdaTrifurcacion=Nothing,
        rectoTrifurcacion=Nothing
    }

{-| 
    Función que devuelve un laberinto vacío con un camino ('Trifurcacion') 
    sin salida y sin 'Tesoro'. 
-}
laberintoDefault :: Laberinto
laberintoDefault = Laberinto {
    trifurcacionLaberinto = caminoDefault,
    tesoroLaberinto = Nothing
}

{-| 
    Función que recibe un String con la descripción de un tesoro y un laberinto
    indicando qué encontrarán si pasan por alto el tesoro, y devuelve el 'Tesoro'. 
-}
crearTesoro :: String -> Maybe Laberinto -> Tesoro
crearTesoro descripcion laberinto = Tesoro {
    descripcionTesoro=descripcion,
    rectoTesoro=laberinto
}

{-| 
    Función que recibe una 'Trifurcacion', un laberinto y un indicador de cuál camino
    los relaciona (izquierda, derecha, recto), y devuelve una Trifurcacion en la que se
    indique que dicho camino conduce a dicho laberinto. 
-}
unirLaberinto :: Trifurcacion -> Maybe Laberinto -> String -> Trifurcacion
unirLaberinto trifurcacion laberinto camino =
    case camino of
        "izquierda" -> trifurcacion { izquierdaTrifurcacion = laberinto }
        "derecha" -> trifurcacion { derechaTrifurcacion = laberinto }
        "recto" -> trifurcacion { rectoTrifurcacion = laberinto }
        _ -> error "Dirección incorrecta."

{-| 
    Función que construye un laberinto a partir de una ruta. 
-}
construirLaberinto :: Ruta -> Maybe Laberinto
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

        _ -> error "Dirección incorrecta."

{-| 
    Función que, dada una ruta, lo recorre hasta alcanzar una pared (un Nothing). 
    La ruta dada a partir de ese punto se convierte en el laberinto alcanzable 
    a través de esa dirección.
-}
abrirPared :: Maybe Laberinto   {-^ Laberinto a modificar -}
        -> Ruta                 {-^ Ruta a recorrer -}
        -> Maybe Laberinto      {-^ Laberinto modificado -}
abrirPared Nothing _ = Nothing
abrirPared lab [] = lab
abrirPared mlab@(Just laberinto) ruta@(c:cs) =
    let trifurcacion = trifurcacionLaberinto laberinto -- Trifurcación del laberinto
        camino = case c of -- Camino tomado en la dirección `c`
            "izquierda" -> izquierdaTrifurcacion trifurcacion
            "recto" -> rectoTrifurcacion trifurcacion
            "derecha" -> derechaTrifurcacion trifurcacion
            _ -> error "Dirección incorrecta."
        lab = case camino of
            lab'@(Just laberinto') -> abrirPared lab' cs -- Si no hay una pared, seguimos buscando la pared.
            Nothing -> construirLaberinto cs -- Si hay una pared, construimos un laberinto a partir de lo que queda de la ruta.
    in (
        Just laberinto {
            -- Cambiamos la trifurcación, uniendolo con lo que queda
            -- del recorrido.
            trifurcacionLaberinto = unirLaberinto trifurcacion lab c
        }
    )

{-|
    Función que, dada una ruta, coloca una pared (un Nothing) en el punto alcanzable.
-}
crearPared :: Maybe Laberinto   {-^ Laberinto a modificar-}
            -> Ruta             {-^ Ruta a seguir -}
            -> Maybe Laberinto  {-^ Laberinto modificado -}
crearPared Nothing _ = Nothing
-- En el caso en el que llegamos al ultimo camino, lo eliminamos
crearPared _ [] = Nothing
crearPared mlab@(Just laberinto) (c:cs) =
    Just $ laberinto { trifurcacionLaberinto = trifurcacion' }
    where 
         -- trifurcación original
        trifurcacion = trifurcacionLaberinto laberinto
        -- Trifurcación modificada
        trifurcacion' = case c of
            "izquierda" -> trifurcacion {
                izquierdaTrifurcacion = 
                    crearPared (izquierdaTrifurcacion trifurcacion) cs
            }
            "recto" -> trifurcacion {
                rectoTrifurcacion =
                    crearPared (rectoTrifurcacion trifurcacion) cs
            }
            "derecha" -> trifurcacion {
                derechaTrifurcacion =
                    crearPared (derechaTrifurcacion trifurcacion) cs
            }
            _ -> error "Camino incorrecto."

{-|
    Función que, dado un laberinto, una ruta y una
    descripción de tesoro, coloca un tesoro al final de la
    ruta proporcionada, ocultando el laberinto anteriormente
    alcanzable desde esa ruta en caso de que exista.
-}
colocarTesoro :: Maybe Laberinto    {-^ Laberinto a modificar-}
            -> Ruta                 {-^ Ruta a seguir -}
            -> String               {-^ Descripción del tesoro a agregar -}
            -> Maybe Laberinto      {-^ Laberinto modificado -}
colocarTesoro Nothing _ _ = Nothing
-- En el caso en el que llegamos al ultimo camino, lo eliminamos
colocarTesoro a [] desc = 
    Just $ Laberinto {
        trifurcacionLaberinto = caminoDefault,
        tesoroLaberinto = Just (crearTesoro desc a)
    }
colocarTesoro mlab@(Just laberinto) (c:cs) desc =
    Just $ laberinto { 
        trifurcacionLaberinto = trifurcacion'
    }
    where 
         -- trifurcación original
        trifurcacion = trifurcacionLaberinto laberinto
        -- Trifurcación modificada
        trifurcacion' = case c of
            "izquierda" -> trifurcacion {
                izquierdaTrifurcacion = 
                    colocarTesoro (izquierdaTrifurcacion trifurcacion) cs desc
            }
            "recto" -> trifurcacion {
                rectoTrifurcacion =
                    colocarTesoro (rectoTrifurcacion trifurcacion) cs desc
            }
            "derecha" -> trifurcacion {
                derechaTrifurcacion =
                    colocarTesoro (derechaTrifurcacion trifurcacion) cs desc
            }
            _ -> error "Camino incorrecto."

{-|
    Función que, dado un laberinto y una ruta, devuelve un laberinto
    en el cuál se elimina el tesoro que estaba al final de la ruta y se
    reestablece el laberinto anteriormente alcanzable desde ese punto.
-}
quitarTesoro :: Maybe Laberinto     {-^ Laberinto a modificar-}
            -> Ruta                 {-^ Ruta a seguir -}
            -> Maybe Laberinto      {-^ Laberinto modificado -}
quitarTesoro Nothing _ = Nothing
-- En el caso en el que llegamos al ultimo camino, lo eliminamos
quitarTesoro (Just lab) [] = 
    case tesoroLaberinto lab of
        Nothing -> Just laberintoDefault
        Just tesoro -> rectoTesoro tesoro 
quitarTesoro mlab@(Just laberinto) (c:cs) =
    Just $ laberinto { 
        trifurcacionLaberinto = trifurcacion'
    }
    where 
         -- trifurcación original
        trifurcacion = trifurcacionLaberinto laberinto
        -- Trifurcación modificada
        trifurcacion' = case c of
            "izquierda" -> trifurcacion {
                izquierdaTrifurcacion = 
                    quitarTesoro (izquierdaTrifurcacion trifurcacion) cs
            }
            "recto" -> trifurcacion {
                rectoTrifurcacion =
                    quitarTesoro (rectoTrifurcacion trifurcacion) cs
            }
            "derecha" -> trifurcacion {
                derechaTrifurcacion =
                    quitarTesoro (derechaTrifurcacion trifurcacion) cs
            }
            _ -> error "Camino incorrecto."

{-|
    Función que devuelve el tesoro existente en una ruta
    de un Laberinto.
-}
obtenerTesoro :: Maybe Laberinto -> Ruta -> Maybe Tesoro
obtenerTesoro Nothing _ = Nothing
obtenerTesoro (Just laberinto) [] = 
    tesoroLaberinto laberinto
obtenerTesoro (Just lab) (c:cs) = 
    case c of
        "izquierda" -> obtenerTesoro (izquierdaTrifurcacion trifurcacion) cs
        "derecha" -> obtenerTesoro (derechaTrifurcacion trifurcacion) cs
        "recto" -> obtenerTesoro (rectoTrifurcacion trifurcacion) cs
        _ -> error "Camino incorrecto"
    where
        trifurcacion = trifurcacionLaberinto lab


--------------------------------------------------------------------------------------------
-- * Funciones de Acceso
--
-- $acceso
-- Este apartado incluye funciones que permiten recorrer los Laberintos y seguir
-- rutas dentro de su configuración.

{-| 
    Función que recibe un laberinto y una ruta y devuelve el laberinto que comienza en el
    punto al que conduce esa ruta 
-}
recorrer :: Maybe Laberinto -> Ruta -> Maybe Laberinto
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
                _ -> error "Dirección incorrecta."

            -- En caso de encontrarse con una pared, ignorar el paso actual
            caminoEscogido' = case caminoEscogido of
                Nothing -> lab -- ignorar paso
                l -> l

{-| 
    Función que recibe un laberinto y devuelve el laberinto que se encuentra 
    al voltear a la izquierda en el punto dado.
-}
voltearIquierda :: Maybe Laberinto -> Maybe Laberinto
voltearIquierda lab = recorrer lab ["izquierda"]

{-| 
    Función que recibe un laberinto y devuelve el laberinto que se encuentra 
    al voltear a la derecha en el punto dado.
-}
voltearDerecha :: Maybe Laberinto -> Maybe Laberinto
voltearDerecha lab = recorrer lab ["derecha"]

{-| 
    Función que recibe un laberinto y devuelve el laberinto que se encuentra 
    al seguir recto en el punto dado.
-}
seguirRecto :: Maybe Laberinto -> Maybe Laberinto
seguirRecto lab = recorrer lab ["recto"]