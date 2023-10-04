import CargaMuestraBike
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


main :: IO ()
main = do
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

mains :: IO ()
mains = do
    -- Cambia la ruta del archivo a la ubicación de tu archivo de datos
    let archivoAlquiler = "../Data App/alquileres.txt"

    -- Carga los alquileres desde el archivo
    alquileres <- leerArchivoAlquiler archivoAlquiler

    let top5 = top5ParqueosMasViajes alquileres

    putStrLn "Top 5 de parqueos con más viajes (salida + destino):"
    mapM_ (\(parqueo, viajes) -> putStrLn $ "Parqueo " ++ parqueo ++ ": " ++ show viajes ++ " viajes") top5




-- Función para calcular la distancia total recorrida por un usuario (cedula)
calcularDistanciaRecorridaPorUsuario :: String -> FilePath -> IO Float
calcularDistanciaRecorridaPorUsuario cedula archivo = do
    contenido <- readFile archivo
    let lineas = lines contenido
    let alquileres = map (Alquilar.creaAlquiler . separaPorComasAlq) lineas
    let alquileresUsuario = filter (\alquiler -> getCedula alquiler == cedula) alquileres
    let parqueosSalidaLlegada = [(getParqueoSalida alquiler, getParqueoLlegada alquiler) | alquiler <- alquileresUsuario]
    distancias <- sequence [CargaParqueos.calcularDistanciaEntreParqueos salida llegada archivo | (salida, llegada) <- parqueosSalidaLlegada]
    return (sum (map (\(Just d) -> d) distancias) / 1000.0)  -- Convertir a kilómetros

-- Función para obtener el top 3 de usuarios con más kilómetros recorridos
top3UsuariosMasKilometros :: FilePath -> IO ()
top3UsuariosMasKilometros archivo = do
    contenido <- readFile archivo
    let lineas = lines contenido
    let alquileres = map (Alquilar.creaAlquiler . (\x -> (x, "")) . separaPorComasAlq) lineas
    let cedulas = groupBy ((==) `on` getCedula) (sortOn getCedula alquileres)
    let usuariosConDistancias = [(cedula, calcularDistanciaRecorridaPorUsuario cedula archivo) | cedula <- map (getCedula . head) cedulas]
    usuariosConDistanciasOrdenados <- sequence usuariosConDistancias
    let top3 = take 3 (sortOn (negate . snd) usuariosConDistanciasOrdenados)
    putStrLn "Top 3 de usuarios con más kilómetros recorridos:"
    mapM_ (\(cedula, distancia) -> putStrLn $ "Cedula: " ++ cedula ++ ", Kilómetros Recorridos: " ++ show distancia) top3

mainf :: IO ()
mainf = do
    let archivo = "../App Data/alquileres.txt"  -- Cambiar a la ruta de tu archivo de alquileres
    top3UsuariosMasKilometros archivo