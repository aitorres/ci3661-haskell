import Laberinto
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import qualified Control.Monad.State as St
 
type LaberintoState = St.StateT Laberinto IO (Maybe Laberinto)

{-Imprimir las opciones del usuario-}
opciones :: IO ()
opciones = do
    putStrLn "El sabio del laberinto"
    putStrLn "Opciones:"
    putStrLn "1: Comenzar a hablar de un laberinto nuevo"
    putStrLn "2: Preguntar ruta"
    putStrLn "3: Reportar pared abierta"
    putStrLn "4: Reportar derrumbe"
    putStrLn "5: Reportar tesoro tomado"
    putStrLn "6: Reportar tesoro hallado"
    putStrLn "7: Dar nombre al laberinto"
    putStrLn "8: Hablar de un laberinto de nombre conocido"
    putStrLn "9: Imprimir opciones"
    putStrLn "10: Salir\n"

{- Loop infinito para leer las opciones del usuario -}
infi :: LaberintoState
infi = do
    opcion <- lift getLine -- leemos la opcion
    case opcion of
        "1" -> do
            laberintoNuevo
            infi
        "9" -> do
            lift opciones
            infi
        "10" -> do
            lift $ putStrLn "Chao viajero"
            return Nothing
        _ -> infi
        
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

main :: IO ()
main = do
    opciones
    St.runStateT infi laberintoDefault
    return ()

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

{- Función que crea un laberinto nuevo a partir de una ruta. -}
laberintoNuevo :: LaberintoState
laberintoNuevo = do
    lift $ putStrLn "Escribe la ruta separada por espacios. Ejemplo: derecha izquierda derecha recto."
    rutaStr <- lift getLine -- Leemos la ruta

    -- Usamos el nuevo laberinto
    let newLab = fromMaybe laberintoDefault (construirLaberinto (words rutaStr))
    St.put $ newLab
    lift $ putStrLn $ "Nuevo laberinto: " ++ show newLab
    return Nothing