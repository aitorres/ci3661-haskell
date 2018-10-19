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
    putStrLn "10: Salir"

infi :: IO ()
infi = do
    opciones
    opcion <- getLine
    if (opcion /= "10") then infi
    else putStrLn "Chao"

main :: IO()
main = do
    infi