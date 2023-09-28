module CargaMuestraBike where
import FuncionesRelevantes
import System.IO
import qualified Control.Monad


-- Estructura bicicletas
type CodigoBicicleta = String
type TipoBicicleta = String
type UbicacionBicicleta = String

data Bicicleta = Bicicleta {
  codigoBicicleta :: CodigoBicicleta,
  tipoBicicleta :: TipoBicicleta,
  ubicacionBicicleta :: UbicacionBicicleta,
  enTransito :: Bool  -- Nuevo campo para rastrear si la bicicleta está en tránsito
}


-- Constructor bicicletas
crearBicicleta :: CodigoBicicleta -> TipoBicicleta -> UbicacionBicicleta -> Bicicleta
crearBicicleta codigo tipo ubicacion = Bicicleta codigo tipo ubicacion False


getCodigo :: Bicicleta -> CodigoBicicleta
getCodigo (Bicicleta codigo _ _ _) = codigo

getTipo :: Bicicleta -> TipoBicicleta
getTipo (Bicicleta _ tipo _ _) = tipo

getUbicacion :: Bicicleta -> UbicacionBicicleta
getUbicacion (Bicicleta _ _ ubicacion _) = ubicacion

getEnTransito :: Bicicleta -> Bool
getEnTransito (Bicicleta _ _ _ enTransito) = enTransito



-- Muestra los detalles de una bicicleta en la consola
-- E: una bicicleta
-- S: N/A
-- R: N/A

mostrarBicicleta :: Bicicleta -> IO ()
mostrarBicicleta bicicleta = do
    let codigo = codigoBicicleta bicicleta
        tipo = tipoBicicleta bicicleta
        ubicacion = ubicacionBicicleta bicicleta
    putStrLn $ "Código de Bicicleta: " ++ codigo
    putStrLn $ "Tipo de Bicicleta: " ++ tipo
    putStrLn $ "Ubicación de Bicicleta: " ++ ubicacion



-- Muestra una bicicleta si su ubicación coincide con el nombre del parqueo proporcionado
-- E: una bicicleta, el nombre de un parqueo
-- S: N/A
-- R: N/A

mostrarBiciParqueo :: Bicicleta -> String -> IO ()
mostrarBiciParqueo bicicleta nombreParqueo = do
    let codigo = codigoBicicleta bicicleta
        tipo = tipoBicicleta bicicleta
        ubicacion = ubicacionBicicleta bicicleta
    Control.Monad.when (ubicacion == nombreParqueo) $ do
            putStrLn $ "Código de Bicicleta: " ++ codigo
            putStrLn $ "Tipo de Bicicleta: " ++ tipo
            putStrLn $ "Ubicación de Bicicleta: " ++ ubicacion


-- Muestra todas las bicicletas del sistema
-- Entrada: una lista de bicicletas
-- Salida: N/A
-- Restricciones: N/A
mostrarBicicletas :: [Bicicleta] -> IO ()
mostrarBicicletas [] = return ()
mostrarBicicletas (bicicleta:restoBicicletas) = do
    mostrarBicicleta bicicleta
    mostrarBicicletas restoBicicletas


-- Muestra las bicicletas de un parqueo
-- Entrada: una lista de bicicletas, nombre del parqueo
-- Salida: N/A
-- Restricciones: N/A
mostrarBicicletasEnParqueo :: [Bicicleta] -> String -> IO ()
mostrarBicicletasEnParqueo [] _ = return ()
mostrarBicicletasEnParqueo (bicicleta:restoBicicletas) nombreParqueo = do
    mostrarBiciParqueo bicicleta nombreParqueo
    mostrarBicicletasEnParqueo restoBicicletas nombreParqueo



-- Convierte una lista de cadenas en una lista de bicicletas
-- Entrada: una lista de cadenas que representan las bicicletas en formato de texto
-- Salida: una lista de bicicletas

separaBicicletas :: [[Char]] -> [Bicicleta]
separaBicicletas [] = []  -- Caso base: cuando la lista está vacía, retornamos una lista vacía de bicicletas
separaBicicletas (biciTexto:resto) =
    let
        -- Separamos el texto de la bicicleta en una lista de elementos usando comas como delimitadores
        elementosBici = separaPorComas (biciTexto, "")
        -- Desempacar los valores necesarios
        [codigo, tipo, ubicacion] = elementosBici
        -- Crear una instancia de Bicicleta
        bicicleta = crearBicicleta codigo tipo ubicacion
    in
        -- Agregar la bicicleta a la lista
        bicicleta : separaBicicletas resto



--lee el archivo de bicicletas
--E: la ruta del archivo
--S: retorna una lista con bicicletas
leerArchivoBicicletas :: FilePath -> IO [Bicicleta]
leerArchivoBicicletas archivo = do
    contenido <- readFile archivo
    let bicicletas = separaBicicletas (lines contenido)
    return bicicletas


-- Muestra una bicicleta si no está en tránsito
-- Entrada: una bicicleta
-- Salida: N/A
-- Restricciones: N/A
mostrarBiciDisponible :: Bicicleta -> IO ()
mostrarBiciDisponible bicicleta =
    let
        codigo = getCodigo bicicleta
        tipo = getTipo bicicleta
        ubicacion = getUbicacion bicicleta
    in
        (Control.Monad.when (ubicacion /= "transito") $ putStrLn $ "Código: " ++ codigo ++ ", Tipo: " ++ tipo ++ ", Ubicación: " ++ ubicacion)

-- Muestra todas las bicicletas disponibles (que no están en tránsito)
-- Entrada: una lista de bicicletas
-- Salida: N/A
-- Restricciones: N/A
mostrarBicisDisponibles :: [Bicicleta] -> IO ()
mostrarBicisDisponibles [] = return ()  -- Caso base: cuando la lista de bicicletas está vacía, no hay nada que mostrar
mostrarBicisDisponibles (bicicleta:restoBicicletas) = do
    mostrarBiciDisponible bicicleta  -- Muestra la bicicleta si no está en tránsito
    mostrarBicisDisponibles restoBicicletas  -- Llama recursivamente a la función con el resto de bicicletas



-- Obtiene una bicicleta a partir de su código, dado una lista de bicicletas
-- Entrada: Código de la bicicleta, lista de bicicletas
-- Salida: Una bicicleta que coincide con el código proporcionado
getBicicleta :: String -> [Bicicleta] -> Bicicleta
getBicicleta codigoBicicleta listaBicicletas = do
    let codigoActual = getCodigo (head listaBicicletas)

    if codigoActual == codigoBicicleta then
        head listaBicicletas
    else
        getBicicleta codigoBicicleta (tail listaBicicletas)


-- Actualiza la ubicación de una bicicleta, ya sea a tránsito o al nombre de un parqueo
-- Entrada: lista de bicicletas, la bicicleta a cambiar, la ubicación a colocar
-- Salida: retorna la lista de bicicletas ya actualizada
actualizarUbicacionBicicleta :: [Bicicleta] -> Bicicleta -> String -> [Bicicleta]
actualizarUbicacionBicicleta [] _ _ = []  -- Caso base: si la lista de bicicletas está vacía, retornamos una lista vacía
actualizarUbicacionBicicleta (bici:restoBicicletas) bicicleta ubicacion
    | getCodigo bici == getCodigo bicicleta =  -- Si encontramos la bicicleta que queremos cambiar
        let
            tipoBici = getTipo bicicleta
            nuevaBici = crearBicicleta (getCodigo bicicleta) tipoBici ubicacion
        in
            nuevaBici : restoBicicletas  -- Agregamos la bicicleta actualizada y el resto de la lista
    | otherwise =
        bici : actualizarUbicacionBicicleta restoBicicletas bicicleta ubicacion  -- Mantenemos la bicicleta actual y seguimos buscando en el resto de la lista


-- Convierte una lista de bicicletas a una representación de cadena de la lista
-- Entrada: lista de bicicletas, una cadena vacía donde se guardará la representación de cadena
-- Salida: una cadena que representa la lista de bicicletas
bicicletasString :: [Bicicleta] -> String -> String
bicicletasString [] s = s  -- Caso base: cuando la lista de bicicletas está vacía, retornamos la cadena acumulada
bicicletasString bicicletas cadenaAcumulada = do
    let codigo = getCodigo (head bicicletas)
        tipo = getTipo (head bicicletas)
        ubicacion = getUbicacion (head bicicletas)
        nuevaBici = codigo ++ "," ++ tipo ++ "," ++ ubicacion ++ "\n"

    bicicletasString (tail bicicletas) (cadenaAcumulada ++ nuevaBici)  -- Agregamos la representación de cadena de la bicicleta actual a la cadena acumulada y continuamos con el resto de la lista


-- Sobrescribe el archivo con los nuevos datos actualizados
-- Entrada: ruta del archivo y los datos a escribir
-- Salida: N/A
escribirNuevosDatos :: String -> String -> IO ()
escribirNuevosDatos ruta datos = do
    writeFile ruta datos  -- Escribe los nuevos datos en el archivo
    return ()  -- No hay valor de retorno, simplemente se ejecuta la acción de escritura
