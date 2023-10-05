module Estadisticas where
import CargaMuestraBike ()
import Alquilar
import Data.List
import Data.Function (on)
import FuncionesRelevantes
import CargaParqueos
-- Función encargada de crear un alquiler a partir de una lista de elementos
creaAlquiler :: [String] -> Alquiler
creaAlquiler elemento = Alquiler (read (elemento !! 0) :: Integer) (elemento !! 1) (elemento !! 2) (elemento !! 3) (elemento !! 4) (elemento !! 5)

-- Función para obtener el "Top 5" de bicicletas con más viajes
top5BicicletasMasViajes :: [Alquiler] -> [(Bici, Int)]
top5BicicletasMasViajes alquileres =
    let contarViajesPorBicicleta = map (\bici -> (bici, length (filter (\a -> getBici a == bici) alquileres))) (nub (map getBici alquileres))
    in take 5 (sortBy (flip compare `on` snd) contarViajesPorBicicleta)


topBici :: IO ()
topBici = do
    -- Cambia la ruta del archivo a la ubicación de tu archivo de datos
    let archivoAlquiler = "../Data App/alquileres.txt"

    -- Carga los alquileres desde el archivo
    alquileres <- leerArchivoAlquiler archivoAlquiler

    let top5 = top5BicicletasMasViajes alquileres

    putStrLn "\n\nTop 5 de bicicletas con más viajes:"
    mapM_ (\(bici, viajes) -> putStrLn $ "Bicicleta " ++ bici ++ ": " ++ show viajes ++ " viajes") top5


-- Función para obtener el "Top 5" de parqueos con más viajes (salida + destino)
top5ParqueosMasViajes :: [Alquiler] -> [(ParqueoSalida, Int)]
top5ParqueosMasViajes alquileres =
    let contarViajesPorParqueo = map (\parqueo -> (parqueo, length (filter (\a -> getParqueoSalida a == parqueo || getParqueoLlegada a == parqueo) alquileres))) (nub (map getParqueoSalida alquileres))
    in take 5 (sortBy (flip compare `on` snd) contarViajesPorParqueo)

topParqueo :: IO ()
topParqueo = do
    -- Cambia la ruta del archivo a la ubicación de tu archivo de datos
    let archivoAlquiler = "../Data App/alquileres.txt"

    -- Carga los alquileres desde el archivo
    alquileres <- leerArchivoAlquiler archivoAlquiler

    let top5 = top5ParqueosMasViajes alquileres

    putStrLn "\n\nTop 5 de parqueos con más viajes (salida + destino):"
    mapM_ (\(parqueo, viajes) -> putStrLn $ "Parqueo " ++ parqueo ++ ": " ++ show viajes ++ " viajes") top5


