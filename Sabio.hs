{-|
Module      : Sabio (Main)
Description : Cliente principal que permite interactuar con el usuario, recorrer rutas y modificar el estado del laberinto.
License     : MIT
Maintainer  : gustavoaca1997@gmail.com, andresitorresm@gmail.com
Stability   : stable
Portability : POSIX

Cliente principal que permite interactuar con el usuario, recibiendo rutas e indicando qué se
encuentra al seguirlas, permitiéndo además modificar el estado del laberinto y de las rutas
parcialmente recorridas.
-}

module Main where
import Laberinto
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.IO
import System.Directory
import System.Exit
import qualified Control.Monad.State as St

--------------------------------------------------------------------------------------------
-- * Tipos de datos
--
-- $tiposDeDatos
-- El cliente principal únicamente define un tipo de datos, LaberintoState,
-- utilizando el monad transformer StateT para manejar el estado del laberinto
-- y la ruta entre operaciones IO.

-- | Monad Transformer que permite mantener el estado de la ruta
-- y el laberinto.
type LaberintoState = St.StateT (Laberinto, Ruta) IO ()

--------------------------------------------------------------------------------------------
-- * Funciones principales
--
-- $funcionesPrincipales
-- Las funciones principales se encargan explícitamente del manejo del cliente,
-- lectura de opciones (comandos) y llamadas a las funciones respectivas
-- para interpretar los comandos del usuario.

{-| 
    Función principal a ejecutar durante la ejecución del cliente. 
-}
main :: IO ()
main = do
    putStrLn "El Sabio del Laberinto"
    St.runStateT opciones (laberintoDefault, [])
    St.runStateT loopInfinito (laberintoDefault, [])
    return ()

{-|
    Función que se encarga de imprimir en pantalla las distintas opciones
    del cliente, tomando en cuenta el estado actual de la ruta. 
-}
opciones :: LaberintoState
opciones = do
    (curLab, curRuta) <- St.get -- obtenemos el estado actual

    -- Imprimimos las opciones
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

{-| 
    Loop infinito que se encarga de leer la entrada del usuario correspondiente
    a las opciones del menú, y procesar las acciones que requiere.
-}
loopInfinito :: LaberintoState
loopInfinito = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    lift $ putStr "Opción: "
    lift $ hFlush stdout
    opcion <- lift getLine -- Preguntamos y leemos la opcion
    case opcion of
        "1" -> do -- Comenzar a hablar de un laberinto nuevo
            laberintoNuevo -- creamos nuevo laberinto

        "2" -> do -- Preguntar Ruta
            rutaStr <- lift obtenerRuta
            let ruta = words rutaStr -- Obtenemos los caminos
            St.put $ (curLab, ruta) -- Actualizamos la ruta actual
            recorrerRuta

        "2.1" -> case curRuta of -- Continuar ruta
            [] -> do
                lift $ putStrLn "No hay ruta que seguir." -- Si no hay ruta que seguir
            _ -> do
                rutaStr <- lift obtenerRuta
                let ruta = words rutaStr -- Obtenemos los caminos
                St.put $ (curLab, curRuta ++ ruta) -- Actualizamos la ruta actual
                recorrerRuta

        "3" -> do -- Pared abierta
            reportarParedAbierta

        "4" -> do -- Reportar derrumbe
            reportarDerrumbe

        "5" -> do -- Reportar tesoro hallado
            reportarTesoroTomado

        "6" -> do -- Reportar tesoro hallado
            reportarTesoroHallado

        "7" -> do -- Dar nombre a laberinto
            nombrarLaberinto

        "8" -> do -- Hablar de un laberinto con nombre conocido
            hablarDeLaberinto

        "9" -> do -- Imprimir opciones
            lift $ putStrLn "A continuación, se listan las opciones disponibles.\n"
            opciones

        "10" -> do -- Salir
            lift $ putStrLn "¡Hasta luego, viajero!"
            lift $ exitSuccess
        
        _ -> error "Opción incorrecta"

    loopInfinito -- Repetimos el loop
        
--------------------------------------------------------------------------------------------
-- * Funciones asociadas a opciones del cliente
--
-- $funcionesCliente
-- Las funciones asociadas a opciones del cliente manejan los distintos comandos 
-- que se pueden utilizar en el cliente, manipulando el estado del Laberinto para
-- obtener información o modificar su estructura, de acuerdo a lo que desee el usuario.

{-| 
    Función que crea un laberinto nuevo a partir de una ruta. 
-}
laberintoNuevo :: LaberintoState
laberintoNuevo = do
    rutaStr <- lift obtenerRuta -- Obtenemos la ruta

    let newLab = fromMaybe laberintoDefault (construirLaberinto (words rutaStr)) -- obtenemos el nuevo laberinto
    St.put $ (newLab, []) -- Lo escogemos como el nuevo laberinto

    -- Imprimimos nuevo laberinto
    lift $ putStrLn "Laberinto creado.\n"

{-| 
    Función que recorre el laberinto siguiendo una ruta y reportando qué
    se encuentra al final de ella.
-}
recorrerRuta :: LaberintoState
recorrerRuta = do
    (curLab, curRuta) <- St.get -- Actual estado
    let ret = recorrer (Just curLab) curRuta -- Recorremos el laberinto siguiendo la ruta
    case ret of
        Nothing -> do
            lift $ putStrLn "No hay laberinto.\n"
            St.put (curLab, [])

        Just lab -> case (tesoroLaberinto lab) of
            Just tesoro -> do -- Hay tesoro
                lift $ putStrLn $ "Se ha encontrado un tesoro: " ++ descripcionTesoro tesoro ++ "\n"
                St.put (curLab, [])

            Nothing -> -- No hay tesoro
                case (trifurcacionLaberinto lab) of

                    -- Camino sin salida
                    Trifurcacion Nothing Nothing Nothing -> do
                        lift $ putStrLn "Se ha llegado a un camino sin salida.\n"
                        St.put (curLab, [])

                    -- Camino normal
                    _ -> lift $ putStrLn $ "No se ha llegado ni a un camino sin salida ni a un tesoro. " ++
                        "Para mostrar las opciones de nuevo, presiona 9. \n"           

{-| 
    Si esta opción es seleccionada, se recibe un camino, luego se
    recorre el camino hasta alcanzar una pared (un Nothing). La ruta dada a partir de ese
    momento se convierte en el laberinto alcanzable por esa dirección.
-}
reportarParedAbierta :: LaberintoState
reportarParedAbierta = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    rutaStr <- lift obtenerRuta
    let ruta = words rutaStr
    St.put $ ( fromJust $ abrirPared (Just curLab) ruta, curRuta )    -- Actualizamos el estado

{-| 
    Si esta opción es seleccionada, se recibe un camino, luego se
    recorre el camino para colocar, al final del mismo, un tesoro. 
    La ruta dada por el "seguir recto" del tesoro se agrega
    como extensión del laberinto.
-}
reportarTesoroHallado :: LaberintoState
reportarTesoroHallado = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    rutaStr <- lift obtenerRuta
    let ruta = words rutaStr
    case (obtenerTesoro (Just curLab) ruta) of
        Just _ -> do
            lift $ putStrLn 
                ("En la ubicación escogida, ya existe un tesoro. " ++ 
                "¡No puedes agregar otro (pero puedes hacerte con una fortuna)!\n")
        Nothing -> do        
            lift $ putStrLn "Escribe la descripción del tesoro."
            lift $ putStr "Descripción: "
            lift $ hFlush stdout
            descTes <- lift $ getLine
            St.put $ ( fromJust $ colocarTesoro (Just curLab) ruta descTes, curRuta )    -- Actualizamos el estado
            lift $ putStrLn "El tesoro descrito ha sido colocado en la ruta seleccionada.\n"

{-| 
    Si esta opción es seleccionada, se recibe un camino, luego se
    recorre el camino para tomar, al final del mismo, un tesoro en caso
    de que exista. Al tomar el tesoro, se anexa al laberinto en ese lugar 
    la ruta dada por el "seguir recto" del tesoro.
-}
reportarTesoroTomado :: LaberintoState
reportarTesoroTomado = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    rutaStr <- lift obtenerRuta
    let ruta = words rutaStr
    case (obtenerTesoro (Just curLab) ruta) of
        Nothing -> do
            lift $ putStrLn 
                "Mala suerte, ¡no hallaste ningún tesoro en esa ruta!.\n"
        Just tesoro -> do        
            lift $ putStrLn
                ("Encontraste el siguiente tesoro: " ++ (descripcionTesoro tesoro) ++
                " y ahora tienes disponible un nuevo recorrido en el laberinto.\n")
            St.put $ ( fromJust $ quitarTesoro (Just curLab) ruta, curRuta )    -- Actualizamos el estado

{-|
    Función que pregunta al usuario por un nombre de manera interactiva y
    almacena en un archivo de ese nombre el 'Laberinto' actual.
-}
nombrarLaberinto :: LaberintoState
nombrarLaberinto = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    lift $ putStrLn "Introduce a continuación el nombre que quieres darle al Laberinto: "
    lift $ putStr "Nombre: "
    lift $ hFlush stdout
    nombreArchivo <- lift getLine -- Obtenemos el nombre del archivo
    lift $ writeFile nombreArchivo (show curLab) -- Escribimos el Laberinto en el archivo
    lift $ putStrLn ("El Laberinto ha recibido correctamente el nombre " ++ nombreArchivo ++ "\n")

{-|
    Función que pregunta al usuario por un nombre de manera interactiva e
    intenta leer un archivo de dicho nombre, para interpretarlo como
    un 'Laberinto'.
-}
hablarDeLaberinto :: LaberintoState
hablarDeLaberinto = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    lift $ putStrLn "Introduce a continuación el nombre del Laberinto del que quieres hablar: "
    lift $ putStr "Nombre: "
    lift $ hFlush stdout
    nombreArchivo <- lift getLine-- Obtenemos el nombre del archivo
    existeAchivo <- lift $ doesFileExist nombreArchivo
    case existeAchivo of
        True -> do
            laberintoLeido <- lift $ readFile nombreArchivo -- Leemos el contenido del archivo
            St.put $ ((read laberintoLeido) :: Laberinto, []) -- Parseamos el archivo como Laberinto
            lift $ putStrLn ("Ahora estamos hablando del Laberinto " ++ nombreArchivo ++ "\n")
        False ->
            lift $ putStrLn ("No existe un archivo llamado " ++ nombreArchivo ++ " por lo que no podemos hablar de tal Laberinto.\n")

{-| 
    Se recibe un camino y una dirección (izquierda, derecha o recto). 
    Se sigue el laberinto hasta ese punto y se elimina el laberinto 
    alcanzable en la dirección dada.
-}
reportarDerrumbe :: LaberintoState
reportarDerrumbe = do
    (curLab, curRuta) <- St.get -- Se obtiene el estado actual

    -- Leemos la ruta
    rutaStr <- lift obtenerRuta
    let ruta = words rutaStr

    St.put $ (fromJust $ crearPared (Just curLab) ruta, curRuta) -- Actualizamos laberinto

--------------------------------------------------------------------------------------------
-- * Funciones auxiliares
--
-- $funcionesAuxiliares
-- Funciones misceláneas que se utilizan en el cliente para modularizar el código (DRY).

{-| 
    Función que pide al usuario una ruta de manera interactiva
    y la devuelve. 
-}
obtenerRuta :: IO (String)
obtenerRuta = do
    putStrLn "Escribe la ruta separada por espacios (Ejemplo: derecha izquierda derecha recto)."
    putStr "Ruta: "
    hFlush stdout
    rutaStr <- getLine -- Leemos la ruta
    return (rutaStr) -- Devolvemos la ruta leída