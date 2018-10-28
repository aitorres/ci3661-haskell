{-|
Module      : Main
License     : MIT
Maintainer  : gustavoaca1997@gmail.com, andresitorresm@gmail.com
El programa principal que permite interactuar con el usuario, recibiendo rutas e indicando qué se
encuentra al seguirlas
-}

module Main where
import Laberinto
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.IO
import qualified Control.Monad.State as St

-- * Tipos de datos

-- | Monad Transformer que permite mantener el estado de la ruta
-- y el laberinto.
type LaberintoState = St.StateT (Laberinto, Ruta) IO ()

-- * Funciones 

{-| Imprimir las opciones del usuario-}
opciones :: LaberintoState
opciones = do
    (curLab, curRuta) <- St.get -- obtenemos el estado actual

    -- Imprimimos las opciones
    lift $ putStrLn "El sabio del laberinto"
    lift $ putStrLn "Opciones:"
    lift $ putStrLn "1: Comenzar a hablar de un laberinto nuevo"

    case curRuta of
        [] -> lift $ putStrLn "2: Preguntar ruta" -- Si no hay ruta que seguir
        _ -> do
            lift $ putStrLn "2: Preguntar ruta nueva" -- Preguntar por nueva ruta
            lift $ putStrLn "2.1: Continuar ruta" -- Continuar recorriendo ruta actual

    lift $ putStrLn "3: Reportar pared abierta"
    lift $ putStrLn "4: Reportar derrumbe"
    lift $ putStrLn "5: Reportar tesoro tomado"
    lift $ putStrLn "6: Reportar tesoro hallado"
    lift $ putStrLn "7: Dar nombre al laberinto"
    lift $ putStrLn "8: Hablar de un laberinto de nombre conocido"
    lift $ putStrLn "9: Imprimir opciones"
    lift $ putStrLn "10: Salir\n"

{- |Loop infinito para leer las opciones del usuario -}
infi :: LaberintoState
infi = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    lift $ putStr "Opción: "
    lift $ hFlush stdout
    opcion <- lift getLine -- leemos la opcion
    case opcion of
        "1" -> do -- Comenzar a hablar de un laberinto nuevo
            laberintoNuevo -- creamos nuevo laberinto
            infi -- repetimos loop

        "2" -> do -- Preguntar Ruta
            lift $ putStrLn "Escribe la ruta separada por espacios (Ejemplo: derecha izquierda derecha recto)."
            rutaStr <- lift obtenerRuta
            let ruta = words rutaStr -- Obtenemos los caminos
            St.put $ (curLab, ruta) -- Actualizamos la ruta actual
            recorrerRuta
            infi -- repetimos loop

        "3" -> do -- Pared abierta
            paredAbierta
            infi

        "4" -> do -- Reportar derrumbe
            reportarDerrumbe
            infi

        "2.1" -> case curRuta of -- Continuar ruta
            [] -> do
                lift $ putStrLn "No hay ruta que seguir." -- Si no hay ruta que seguir
                infi -- repetimos loop
            _ -> do
                lift $ putStrLn "Escribe la ruta separada por espacios (Ejemplo: derecha izquierda derecha recto)."
                rutaStr <- lift obtenerRuta
                let ruta = words rutaStr -- Obtenemos los caminos
                St.put $ (curLab, curRuta ++ ruta) -- Actualizamos la ruta actual
                recorrerRuta
                infi -- repetimos loop

        "7" -> do
            lift $ putStrLn "Introduce a continuación el nombre que quieres darle al Laberinto: "
            lift $ putStr "Nombre: "
            lift $ hFlush stdout
            nombreArchivo <- lift getLine -- Obtenemos el nombre del archivo
            lift $ writeFile nombreArchivo (show curLab) -- Escribimos el Laberinto en el archivo
            lift $ putStrLn ("El Laberinto ha recibido correctamente el nombre " ++ nombreArchivo)
            infi

        "8" -> do
            lift $ putStrLn "Introduce a continuación el nombre del Laberinto del que quieres hablar: "
            lift $ putStr "Nombre: "
            lift $ hFlush stdout
            nombreArchivo <- lift getLine-- Obtenemos el nombre del archivo
            laberintoLeido <- lift $ readFile nombreArchivo -- Leemos el contenido del archivo
            St.put $ ((read laberintoLeido) :: Laberinto, []) -- Parseamos el archivo como Laberinto
            lift $ putStrLn ("Ahora estamos hablando del Laberinto " ++ nombreArchivo)
            infi

        "9" -> do -- Imprimir opciones
            opciones
            infi
        "10" -> do -- Salir
            lift $ putStrLn "Chao viajero"
        _ -> error "Opción incorrecta"
        
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
{-| Función principal.-}
main :: IO ()
main = do
    St.runStateT opciones (laberintoDefault, [])
    St.runStateT infi (laberintoDefault, [])
    return ()

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

{- |Función auxiliar que pide y devuelve al usuario una ruta. -}
obtenerRuta :: IO (String)
obtenerRuta = do
    putStr "Ruta: "
    hFlush stdout
    rutaStr <- getLine -- Leemos la ruta
    return (rutaStr)

{- |Función que crea un laberinto nuevo a partir de una ruta. -}
laberintoNuevo :: LaberintoState
laberintoNuevo = do
    lift $ putStrLn "Escribe la ruta separada por espacios (Ejemplo: derecha izquierda derecha recto)."
    rutaStr <- lift obtenerRuta

    let newLab = fromMaybe laberintoDefault (construirLaberinto (words rutaStr)) -- obtenemos el nuevo laberinto
    St.put $ (newLab, []) -- Lo escogemos como el nuevo laberinto

    -- Imprimimos nuevo laberinto
    -- lift $ putStrLn $ "Nuevo laberinto: " ++ show newLab
    lift $ putStrLn "Laberinto creado.\n"

{- |Función que recorre el laberinto siguiendo una ruta -}
recorrerRuta :: LaberintoState
recorrerRuta = do
    (curLab, curRuta) <- St.get -- Actual estado
    let ret = recorrer (Just curLab) curRuta -- Recorremos el laberinto siguiendo la ruta
    case ret of
        Nothing -> do
            lift $ putStrLn "No hay laberinto"
            St.put (curLab, [])

        Just lab -> case (tesoroLaberinto lab) of
            Just tesoro -> do -- Hay tesoro
                lift $ putStrLn $ "Se ha encontrado un tesoro: " ++ show tesoro
                St.put (curLab, [])

            Nothing -> -- No hay tesoro
                case (trifurcacionLaberinto lab) of

                    -- Camino sin salida
                    Trifurcacion Nothing Nothing Nothing -> do
                        lift $ putStrLn "Se ha llegado a un camino sin salida."
                        St.put (curLab, [])

                    -- Camino normal
                    _ -> lift $ putStrLn $ "No se ha llegado ni a un camino sin salida ni a un tesoro. " ++
                        "Para mostrar las opciones de nuevo, presiones 9."
                

{-| Si esta opción es seleccionada, se recibe un camino, luego se
recorre el camino hasta alcanzar una pared (un Nothing). La ruta dada a partir de ese
momento se convierte en el laberinto alcanzable por esa dirección.-}
paredAbierta :: LaberintoState
paredAbierta = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    lift $ putStrLn "Escribe la ruta separada por espacios (Ejemplo: derecha izquierda derecha recto)."
    rutaStr <- lift obtenerRuta
    let ruta = words rutaStr
    St.put $ ( fromJust $ abrirPared (Just curLab) ruta, curRuta )    -- Actualizamos el estado

{- | Se recibe un camino y una dirección (izquierda, derecha o recto). 
Se sigue el laberinto hasta ese punto y se elimina el laberinto 
alcanzable en la dirección dada.-}
reportarDerrumbe :: LaberintoState
reportarDerrumbe = do
    (curLab, curRuta) <- St.get -- Se obtiene el estado actual

    -- Leemos la ruta
    lift $ putStrLn "Escribe la ruta separada por espacios (Ejemplo: derecha izquierda derecha recto)."
    rutaStr <- lift obtenerRuta
    let ruta = words rutaStr

    St.put $ (fromJust $ crearPared (Just curLab) ruta, curRuta) -- Actualizamos laberinto
