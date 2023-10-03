import CargaParqueos
import CargaMuestraBike
-- Valida largo de cedula 
compruebaCedula :: String -> Bool
compruebaCedula cedula = length cedula == 9

-- Función para verificar si existe un parqueo con un nombre dado en la lista de parqueos
existeNombreParque :: [Parqueo] -> NombreParqueo -> Bool
existeNombreParque [] _ = False
existeNombreParque ((Parqueo _ nombre _ _ _ _):parqueosRestantes) targetNombre
    | nombre == targetNombre = True
    | otherwise = existeNombreParque parqueosRestantes targetNombre

alquila :: IO ()
alquila = do
    putStrLn "\n\nIngrese una cédula:"
    cedula <- getLine
    if compruebaCedula cedula 
        then do
            let baseDatos = "../Data App/infoParqueos.txt" --Datos almacenados
            parqueos <- leerArchivo baseDatos
            let baseBicis = "../Data App/bicicletas.txt"
            baseBicis <- CargaMuestraBike.leerArchivoBicis baseBicis
            putStrLn "\n\nIndique el nombre del parqueo de Salida: "
            salida <- getLine
            putStrLn "\n\nIndique el nombre del parqueo de Llegada: "
            llegada <- getLine
            if CargaParqueos.existeNombreAux parqueos salida && CargaParqueos.existeNombreAux parqueos llegada 
                then do
                    putStrLn "\n\n\nMostrando bicicletas disponibles desde el punto de salida:"
                    mostrarBicicletasEnParqueo baseBicis salida
                    putStrLn  "\nIndique el codigo de la bicileta a alquilar: "
                    codigo <- getLine 
                    --validacion de existencia del codigo 
                    if CargaMuestraBike.existeCodigo baseBicis codigo
                        then putStrLn"Existe el codigo"
                        else putStrLn"No existe el codigo"
                    
                else putStrLn "Debe de indicar parqueos registrados en el sistema"
            
        else putStrLn "La cédula debe tener 9 dígitos."

    


