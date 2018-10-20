import Laberinto
import Control.Monad
import qualified Control.Monad.State as St
 
type LaberintoState = St.StateT Laberinto IO (Maybe Laberinto)

opciones :: IO (Maybe Laberinto)
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
    putStrLn "10: Salir"
    return Nothing

infi :: IO (Maybe Laberinto)
infi = do
    opciones
    opcion <- getLine
    if (opcion /= "10") then infi
    else do 
        putStrLn "Chao"
        return Nothing
        

main :: IO (Maybe Laberinto)
main = do
    infi