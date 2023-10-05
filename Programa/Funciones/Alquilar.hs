module Alquilar where
import CargaParqueos
import CargaMuestraBike
import System.IO
import System.Directory --Para Modificacion de archivos
--Estructura de datos para un alquiler 
type IdAlquiler = Integer
type Cedula = String
type ParqueoSalida = String
type ParqueoLlegada = String
type Bici = String
type Estado = String
data Alquiler = Alquiler IdAlquiler Cedula ParqueoSalida ParqueoLlegada Bici Estado

--Funcion encaregada de crear alquiler 
--E: String en lista
--S: Un nuevo alquiler
--R: Debe seguir la estructura de datos de un alquiler
creaAlquiler :: [String] -> Alquiler
creaAlquiler elemento = Alquiler (read (elemento !! 0) :: Integer) (elemento !! 1) (elemento !! 2) (elemento !! 3)(elemento !! 4)(elemento !! 5) 

----  Metodos Accesores
getIdAlquiler :: Alquiler -> IdAlquiler
getIdAlquiler (Alquiler idAlquiler _ _ _ _ _) = idAlquiler
getCedula :: Alquiler -> Cedula
getCedula (Alquiler _ cedula _ _ _ _) = cedula
getParqueoSalida:: Alquiler -> ParqueoSalida
getParqueoSalida(Alquiler _ _ parqueoSalida _ _ _) = parqueoSalida
getParqueoLlegada:: Alquiler -> ParqueoLlegada
getParqueoLlegada(Alquiler _ _ _ parqueoLlegada _ _) = parqueoLlegada
getBici :: Alquiler -> Bici
getBici(Alquiler _ _ _ _ bici _) = bici
getEstado :: Alquiler -> Estado
getEstado(Alquiler _ _ _ _ _ estado) = estado

-- Valida largo de cedula 
-- E: String
-- S: Bool, true si la cedula es de 9 digitos
-- R: enteros
compruebaCedula :: String -> Bool
compruebaCedula cedula = length cedula == 9

-- Función para verificar si existe un parqueo con un nombre dado en la lista de parqueos
-- E: lista de parqueos y Nombre a buscar
-- S: Bool, true si existe
-- R: debe de existir el nombre de consulta 
existeNombreParque :: [Parqueo] -> NombreParqueo -> Bool
existeNombreParque [] _ = False
existeNombreParque ((Parqueo _ nombre _ _ _ _):parqueosRestantes) targetNombre
    | nombre == targetNombre = True
    | otherwise = existeNombreParque parqueosRestantes targetNombre


-- Función para agregar un registro de alquiler a un archivo existente
-- E: CEDULA, PARQUESALIDA, PARQUEOLLEGADA, BICI
-- S: Archivo con los datos entrantes 
-- R: datos entrantes deben respetar estructura de datos del alquiler
creaArchivos :: Cedula -> ParqueoSalida -> ParqueoLlegada -> Bici -> IO ()
creaArchivos cedula parqueoSalida parqueoLlegada bicicleta = do 
    let contenido = show cedula ++ ","
                 ++ parqueoSalida ++ ","
                 ++ parqueoLlegada ++ ","
                 ++ bicicleta 
    let ruta = "../Data App/alquileres.txt"
    appendFile ruta (contenido ++ "\n")  -- Agregar el contenido al archivo existente



-- Función para agregar un ID autoincrementable a cada fila de un archivo con 4 elementos separados por comas
agregarIdsAutoincrementables :: FilePath -> IO ()
agregarIdsAutoincrementables ruta = do
    -- Leer el contenido actual del archivo
    contenidoActual <- readFile ruta

    -- Dividir el contenido en líneas
    let lineas = lines contenidoActual

    -- Función para verificar si una línea tiene exactamente 4 elementos separados por comas
    let tieneCuatroElementos linea = length (separaPorComasAlq (linea, "")) == 4

    -- Crear un nuevo contenido con IDs autoincrementables solo para las líneas con 4 elementos
    let nuevoContenido = unlines [if tieneCuatroElementos linea then show (i + 1) ++ "," ++ linea else linea | (i, linea) <- zip [0..] lineas]

    -- Escribir el nuevo contenido en un archivo temporal
    let rutaTemporal = ruta ++ ".temp"
    writeFile rutaTemporal nuevoContenido

    -- Eliminar el archivo original
    removeFile ruta

    -- Renombrar el archivo temporal al nombre original
    renameFile rutaTemporal ruta

-------- Lectura 
separaPorComasAlq :: ([Char], [Char]) -> [[Char]]
separaPorComasAlq (cadena,temp) = --split
    if cadena == "" then[temp] else
        if (head cadena) == (head ",") then 
            [temp] ++ separaPorComasAlq((tail cadena), "")
        else
            separaPorComasAlq((tail cadena), temp++[(head cadena)])


separaElementosAlquiler :: [[Char]] -> [Alquiler]
separaElementosAlquiler lista = 
    if lista == [] then 
        []
    else 
        [creaAlquiler(separaPorComasAlq((head lista), ""))] ++ separaElementosAlquiler(tail lista)

convierteALineasAlqu :: String -> [String]
convierteALineasAlqu texto = lines texto

leerArchivoAlquiler :: FilePath -> IO [Alquiler]
leerArchivoAlquiler archivo = do
    contenido <- readFile archivo
    let alquileres = separaElementosAlquiler(convierteALineasAlqu contenido)
    return alquileres

---- Funcion principal
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
            let baseAlquileres = "../Data App/alquileres.txt" 

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
                        then do
                            creaArchivos cedula salida llegada codigo 
                            agregarIdsAutoincrementables baseAlquileres
                            putStrLn "Se realizo con exito el alquiler"
                            -- Aqui funcion para poner el estado de la bici cen transito -- 

                        else do 
                            putStrLn"\n\nNo existe el codigo de la bici"
                            alquila
                else do
                    putStrLn "\n\n\nDebe de indicar parqueos registrados en el sistema"
                    alquila
        else do 
            putStrLn "\n\n\nLa cédula debe tener 9 dígitos."
            alquila

    


