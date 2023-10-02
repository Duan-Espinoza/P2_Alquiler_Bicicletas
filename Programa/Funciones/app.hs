---Importaciones librerias para  Paqueo
import System.Directory  --Libreria para verificacion de rutas
import Data.List (isInfixOf) --Libreria para validacion de las provincias 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (elem)
import Control.Exception (try)
import GHC.Read (readField)

-- Seccion de vistas 
vistaMenuInicial = do
    putStrLn "\n\n \tBienvenido"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"

vistaOperativas = do 
    putStrLn "\n\n \tSe encuentra en opciones Operativas"
    putStrLn "1.Información Comercial"
    putStrLn "2.Carga y Mostrar parqueos"
    putStrLn "3.Mostrar y asignar bicicletas"
    putStrLn "4.Cargar Usuarios"
    putStrLn "5. Estadísticas"
    putStrLn "6.Volver"

vistaGenerales = do 
    putStrLn "\n\n \tSe encuentra en las Opciones Generales"
    putStrLn "1.Consultar Bicicletas"
    putStrLn "2.Alquilar"
    putStrLn "3.Facturar"
    putStrLn "4.Volver"

--Seccion funcional de menus
menuGenerales = do 
    putStrLn "\nIndique una de las opciones mostradas: "
    opcion <- getLine
    let op = (read opcion :: Int)
    if null opcion then do 
        putStrLn "Debe de colocar una de las opciones mostradas"
        main 
    else
        if op == 1
            then do 
                putStrLn "\nEn DESAROSLLO "
        else if op == 2 
            then do 
                putStrLn "\nEn DESAROLSDLO "
        else if op == 3
            then do 
                putStrLn "\nEn DESAROLLDO "
        else if op == 4
            then do 
                main
        else 
            do
                putStrLn "Debe de selecccionar una de las opciones mostradas"
                vistaGenerales
                menuGenerales

menuOperativas = do 
    putStrLn "\nIndique una de las opciones mostradas: "
    opcion <- getLine
    let op = (read opcion :: Int)
    if null opcion then do 
        putStrLn "Debe de colocar una de las opciones mostradas"
        main 
    else
        if op == 1
            then do 
                putStrLn "\nEn DESAROSLLO "
        else if op == 2 
            then do 
                -- CARGA Y MUESTRA DE PARQUEOS
                cargaParqueo
        else if op == 3
            then do 
                putStrLn "\nEn DESAROLLDO "
        else if op == 4
            then do 
                putStrLn "\nEn DESAROLLDdO "
        else if op == 5
            then do 
                putStrLn "\nEn DESAROLLDO "
        else if op == 6
            then do 
                main
        else 
            do
                putStrLn "Debe de selecccionar una de las opciones mostradas"
                vistaOperativas
                menuOperativas
-- inicializador 
main = do
    vistaMenuInicial
    putStrLn "\nIndique una de las opciones mostradas: "
    opcion <- getLine
    let op = (read opcion :: Int)
    if null opcion then do 
        putStrLn "Debe de colocar una de las opciones mostradas"
        main 
    else
        if op == 1
            then do  
                vistaOperativas 
                menuOperativas
            
        else if op == 2
            then do
                vistaGenerales
                menuGenerales
        else 
            do 
                putStrLn "Opción no válida"
                main


-- Nota: Para la creación de .exe es en consola con ghc mi_programa.hs para win en linux es ./mi_programa  # En sistemas Unix/Linux
-- ghc -o /ruta/a/la/ubicacion/mi_programa.exe mi_programa.hs EJEMPLO para colocar en direccion X


-------------------------------------------------------------------- Apartado de Carga Parqueos -------------------------------------------------------------------------------------



--       Datos parqueo 
-- Definición de tipos de datos
type IdParqueo = Integer
type NombreParqueo = String
type DireccionParqueo = String
type ProvinciaParqueo = String
type CordenadaX = Float
type CordenadaY = Float
data Parqueo = Parqueo IdParqueo NombreParqueo DireccionParqueo ProvinciaParqueo CordenadaX CordenadaY

-- Funcion que crea archivo con direccion 
creaArchivo = do 
    let ruta = "../Data App/infoParqueos"
    let contenido = "Hola"
    writeFile ruta contenido

-- Consulta la ruta del archivo 
-- E: Una ruta
-- R: Necesita una ruta existente para evaluar 
-- S: 
 :: IO ()
cargaParqueo = do
    putStrLn "\n\n Indique la ruta del archivo a cargar: "
    ruta <- getLine
    validoID <- verificarIDsUnicos ruta --Verificacion del id 

    if null ruta || ruta == "" then do   
        putStrLn "\n\n ---- Debe colocar una dirección válida ----"
        cargaParqueo
    else do 
        existe <- doesPathExist ruta
        if existe
            then do
                putStrLn ("\n\n ---- La ruta " ++ ruta ++ " existe. Se evaluará unicidad de los datos ----") 
                if validoID
                    then do 
                        --Se Valida las provincias 
                        estatusProvincia <- verificarDatos ruta
                        if estatusProvincia
                            then do 
                                putStrLn "\n Los datos son válidos, se van a cargar a la base de datos."
                                parqueos <- leerArchivo ruta
                                putStrLn "\n ------------------------------------------------      Parqueos cargados    ------------------------------------------------ \n"
                                showParqueos parqueos
                                putStrLn "------------------------------------------------------------------------------------------------------------------------------------"
                            else do
                                putStrLn "\n\n ---- El documento incluye provincias inexistentes ----"
                                cargaParqueo

                    else do 
                        putStrLn "Hay IDs repetidos en el archivo. Intenten con otro archivo que no tenga ID repetidos"
                        cargaParqueo
                
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

--Funcion que Verifica ID
verificar :: [String] -> Set String -> Bool
verificar [] _ = True
verificar (linea:resto) idsVistos =
    let id = takeWhile (/= ',') linea
        provincia = obtenerProvincia linea
    in
        if Set.member id idsVistos || not (validarProvincia provincia)
            then False
            else verificar resto (Set.insert id idsVistos)


-- Lista de provincias permitidas
provinciasPermitidas :: [String]
provinciasPermitidas = ["LI", "PU", "GU", "SJ", "AL", "CA", "HE"]

-- Función para validar una provincia
validarProvincia :: String -> Bool
validarProvincia provincia = any (`isInfixOf` provincia) provinciasPermitidas

-- Funcion encargada de verificar datos del txt
verificarDatos :: FilePath -> IO Bool
verificarDatos archivo = do
    contenido <- readFile archivo
    let lineas = lines contenido
    let idsVistos = Set.empty
    let resultado = verificar lineas idsVistos
    return resultado

-- Función para validar una provincia
obtenerProvincia :: String -> String
obtenerProvincia linea = 
    let partes = separaPorComas (linea, "")  -- Llama a tu función separaPorComas
    in
        if length partes >= 4
            then partes !! 3
            else ""


-- Funciones encargadas de creación y Registro de Parqueos
creaParqueo :: [String] -> Parqueo
creaParqueo elemento = Parqueo (read (elemento !! 0) :: Integer) (elemento !! 1) (elemento !! 2) (elemento !! 3)(read (elemento !! 4) :: Float) (read (elemento !! 5) :: Float)

-- Metodos Accesores
getId :: Parqueo -> IdParqueo
getId (Parqueo id _ _ _ _ _) = id
getNombreParqueo :: Parqueo -> NombreParqueo
getNombreParqueo (Parqueo _ nombre _ _ _ _) = nombre
getDireccionParqueo :: Parqueo -> DireccionParqueo
getDireccionParqueo (Parqueo _ _ direccion _ _ _) = direccion
geProvincia :: Parqueo -> ProvinciaParqueo
geProvincia(Parqueo _ _ _ provincia _ _) = provincia
getCordenadaX :: Parqueo -> CordenadaX
getCordenadaX(Parqueo _ _ _ _ cordenada _ ) = cordenada
getCordenadY :: Parqueo -> CordenadaY
getCordenadY(Parqueo _ _ _ _ _ cordenadaY ) = cordenadaY

showParqueo parqueo = 
    let 
        id = getId(parqueo)
        nombre = getNombreParqueo(parqueo)
        direccion = getDireccionParqueo(parqueo)
        provincia = geProvincia(parqueo)
        cordenadaX = getCordenadaX(parqueo)
        cordenadaY = getCordenadY(parqueo)

    in
        "El id es: " ++ show id  ++ ", nombre del Parque: "++ nombre ++ ", su direccion es: " ++ direccion ++ ", provincia: " ++ provincia ++ ", coordenada X: "++ show cordenadaX ++ ", coordenada Y: " ++ show  cordenadaY

showParqueos [] = print("")
showParqueos lista = 
    do 
        print(showParqueo(head lista))
        showParqueos(tail lista)


existeID parqueos = 
    do 
        putStrLn ("Indique un id")
        tempId <- getLine
        let id = (read tempId :: Integer)
        existeIdAux(parqueos, id) 

existeIdAux ([],cedula) = print ("no encontrado")
existeParqueoAux(parqueos, id) = do 
    let primero = (head parqueos)
    let idNew = getId(primero)
    if idNew == id then 
        print "es"
    else
        existeParqueoAux((tail parqueos), id) 

separaPorComas :: ([Char], [Char]) -> [[Char]]
separaPorComas (cadena,temp) = --split
    if cadena == "" then[temp] else
        if (head cadena) == (head ",") then 
            [temp] ++ separaPorComas((tail cadena), "")
        else
            separaPorComas((tail cadena), temp++[(head cadena)])


separaElementos :: [[Char]] -> [Parqueo]
separaElementos lista = 
    if lista == [] then 
        []
    else 
        [creaParqueo(separaPorComas((head lista), ""))] ++ separaElementos(tail lista)

convierteALineas :: String -> [String]
convierteALineas texto = lines texto

leerArchivo :: FilePath -> IO [Parqueo]
leerArchivo archivo = do
    contenido <- readFile archivo
    let parqueos = separaElementos(convierteALineas contenido)
    return parqueos




-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------