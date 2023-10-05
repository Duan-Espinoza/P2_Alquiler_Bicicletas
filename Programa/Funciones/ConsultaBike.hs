module ConsultaBike where
import CargaMuestraBike
import CargaParqueos
import System.IO
import Control.Monad
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

-- Define la función distanciaEuclidiana si no existe en tu código actual
distanciaEuclidiana :: (Float, Float) -> (Float, Float) -> Float
distanciaEuclidiana (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)
-- ... (otras importaciones y definiciones)

-- Función para encontrar el parqueo más cercano y mostrar sus bicicletas
encontrarParqueoCercano :: [(String, (Float, Float))] -> [(String, [Bicicleta])] -> (Float, Float) -> IO ()
encontrarParqueoCercano parqueos bicicletas (x, y) = do
    -- Calcula las distancias entre la posición (x, y) y las coordenadas de los parqueos
    let distancias = map (\(nombre, (xParqueo, yParqueo)) ->
            (nombre, distanciaEuclidiana (x, y) (xParqueo, yParqueo))) parqueos
        -- Encuentra el parqueo más cercano (el de menor distancia)
        parqueoMasCercano = minimumBy (comparing snd) distancias

    putStrLn $ "El parqueo más cercano es: " ++ fst parqueoMasCercano
    putStrLn "Bicicletas en el parqueo más cercano:"
    -- Obtiene las bicicletas del parqueo más cercano
    let bicicletasParqueoMasCercano = fromJust $ lookup (fst parqueoMasCercano) bicicletas
    -- Muestra las bicicletas del parqueo
    mostrarBicicletas bicicletasParqueoMasCercano




mainConsulta archivoParqueos= do
    -- Obtener las ubicaciones de los parqueos desde CargaParqueos
    --let archivoParqueos = "../Data App/infoParqueos.txt"  -- Cambia la ruta según tu ubicación
    parqueos <- leerArchivo archivoParqueos

    -- Convertir la lista de Parqueo a una lista de tuplas (nombre, (x, y))
    let parqueosConCoordenadas = map (\parqueo -> (getNombreParqueo parqueo, (getCordenadaX parqueo, getCordenadY parqueo))) parqueos

    putStrLn "\nIngrese la posición (x, y):"
    x <- readLn
    y <- readLn

    -- Aquí se asume que tienes una función llamada leerArchivoBicicletas para cargar la lista de bicicletas desde el archivo.
    -- La lista de bicicletas se carga desde el archivo "../Data App/infoBicicletas.txt". Asegúrate de definir la función leerArchivoBicicletas adecuadamente.
    let archivoBicicletas = "../Data App/bicicletas.txt"  -- Cambia la ruta según tu ubicación
    bicicletas <- leerArchivoBicicletas archivoBicicletas

    -- Luego, puedes obtener las bicicletas del parqueo más cercano
    let distancias = map (\(nombre, (xParqueo, yParqueo)) -> (nombre, distanciaEuclidiana (x, y) (xParqueo, yParqueo))) parqueosConCoordenadas
        -- Extrae las distancias (segundo elemento de las tuplas) de la lista de tuplas
        distanciasParqueos = map snd distancias
        -- Encuentra el parqueo más cercano
        parqueoMasCercano = fst $ minimumBy (comparing snd) (zip (map fst distancias) distanciasParqueos)
        bicicletasParqueoMasCercano = obtenerBicicletasPorParqueo bicicletas parqueoMasCercano

    putStrLn $ "El parqueo más cercano es: " ++ parqueoMasCercano
    putStrLn "Bicicletas en el parqueo más cercano:"
    mostrarBicicletas bicicletasParqueoMasCercano
