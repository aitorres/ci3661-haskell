{-|
Module      : Main
License     : MIT
Maintainer  : gustavoaca1997@gmail.com
El programa principal que permite interactuar con el usuario, recibiendo rutas e indicando qué se
encuentra al seguirlas
-}

module Main where
import Laberinto
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import qualified Control.Monad.State as St

-- | Lista de direcciones a seguir.
type Ruta = [String]

-- | Monad Transformer que permite mantener el estado de la ruta
-- y el laberinto.
type LaberintoState = St.StateT (Laberinto, Ruta) IO ()

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
    lift $ putStrLn "Ingrese opción:"

{- |Loop infinito para leer las opciones del usuario -}
infi :: LaberintoState
infi = do
    (curLab, curRuta) <- St.get -- Obtenemos el estado actual
    opcion <- lift getLine -- leemos la opcion
    case opcion of
        "1" -> do -- Comenzar a hablar de un laberinto nuevo
            laberintoNuevo -- creamos nuevo laberinto
            infi -- repetimos loop

        "2" -> do -- Preguntar Ruta
            lift $ putStrLn "Escribe la ruta separada por espacios. Ejemplo: derecha izquierda derecha recto."
            rutaStr <- lift getLine -- leemos la ruta
            let ruta = words rutaStr -- Obtenemos los caminos
            St.put $ (curLab, ruta) -- Actualizamos la ruta actual
            recorrerRuta
            infi -- repetimos loop

        "2.1" -> case curRuta of -- Continuar ruta
            [] -> do
                lift $ putStrLn "No hay ruta que seguir." -- Si no hay ruta que seguir
                infi -- repetimos loop
            _ -> do
                lift $ putStrLn "Escribe la ruta separada por espacios. Ejemplo: derecha izquierda derecha recto."
                rutaStr <- lift getLine -- leemos la ruta
                let ruta = words rutaStr -- Obtenemos los caminos
                St.put $ (curLab, curRuta ++ ruta) -- Actualizamos la ruta actual
                recorrerRuta
                infi -- repetimos loop


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

{- |Función que crea un laberinto nuevo a partir de una ruta. -}
laberintoNuevo :: LaberintoState
laberintoNuevo = do
    lift $ putStrLn "Escribe la ruta separada por espacios. Ejemplo: derecha izquierda derecha recto."
    rutaStr <- lift getLine -- Leemos la ruta

    let newLab = fromMaybe laberintoDefault (construirLaberinto (words rutaStr)) -- obtenemos el nuevo laberinto
    St.put $ (newLab, []) -- Lo escogemos como el nuevo laberinto

    -- Imprimimos nuevo laberinto
    -- lift $ putStrLn $ "Nuevo laberinto: " ++ show newLab

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
                

        