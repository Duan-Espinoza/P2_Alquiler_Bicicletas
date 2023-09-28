import System.Directory  --Libreria para verificacion de rutas
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (elem)
--------------- Datos parqueo 

-- Definición de tipos de datos
type IdParqueo = Integer
type NombreParqueo = String
type DireccionParqueo = String
type ProvinciaParqueo = String
type CordenadaX = Float
type CordenadaY = Float

data Parqueo = Parqueo IdParqueo NombreParqueo DireccionParqueo ProvinciaParqueo CordenadaX CordenadaY

creaParqueo :: [String] -> Parqueo
creaParqueo elemento = Parqueo (read (elemento !! 0) :: Integer) (elemento !! 1) (elemento !! 2) (elemento !! 3)(read (elemento !! 4) :: Float) (read (elemento !! 5) :: Float)

getId :: Parqueo -> IdParqueo
getId (Parqueo id _ _ _) = id


-- Funcion que crea archivo con direccion 
creaArchivo = do 
    let ruta = "../Data App/infoParqueos"
    let contenido = "Hola"
    writeFile ruta contenido

-- Consulta la ruta del archivo 
-- E: Una ruta
-- R: Necesita una ruta existente para evaluar 
-- S: Bool, true si existe 
consultaRuta = do
    putStrLn "\nIndique la ruta del archivo a cargar: "
    ruta <- getLine
    validoID <- verificarIDsUnicos ruta --Verificacion del id 
    if null ruta || ruta == "" then do   
        putStrLn "Debe colocar una dirección válida."
        consultaRuta
    else do 
        putStrLn ("Se está inicializando la búsqueda en: " ++  ruta)
        existe <- doesPathExist ruta
        if existe
            then do
                putStrLn ("La ruta " ++ ruta ++ " existe. Se evaluará unicidad de los datos") 
                if validoID
                    then putStrLn "Todos los IDs son únicos."
                    else do 
                        putStrLn "Hay IDs repetidos en el archivo. Intenten con otro archivo que no tenga ID repetidos"
                        consultaRuta
                
            else do
                putStrLn ("La ruta " ++ ruta ++ " no existe.")


-------- Validaciones de datos del archivo 


-- Función para verificar si los IDs son únicos en un archivo
verificarIDsUnicos :: FilePath -> IO Bool
verificarIDsUnicos archivo = do
    contenido <- readFile archivo
    let lineas = lines contenido
    let idsVistos = Set.empty
    let resultado = verificarIDs lineas idsVistos
    return resultado

-- Función auxiliar para verificar IDs únicos
verificarIDs :: [String] -> Set String -> Bool
verificarIDs [] _ = True  -- Todos los IDs son únicos
verificarIDs (linea:resto) idsVistos =
    let id = takeWhile (/= ',') linea  -- Obtiene el ID de la primera línea, es que inicia hasta que encuentre la primera coma
    in
        if Set.member id idsVistos
            then False  -- El ID ya se ha visto, no es único
            else verificarIDs resto (Set.insert id idsVistos)


es = do
    let archivo = "parqueos.txt"  -- Nombre del archivo a verificar 
    resultado <- verificarIDsUnicos archivo
    if resultado
        then putStrLn "Todos los IDs son únicos."
        else putStrLn "Hay IDs repetidos en el archivo."


