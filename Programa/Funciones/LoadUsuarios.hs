module LoadUsuarios where
import FuncionesRelevantes

-- Estructura Usuarios
-- para el constructor
type Cedula = Integer
type NombreUsuario = String
data Usuario = Usuario Cedula NombreUsuario;


-- Constructor usuario
crearUsuario :: [String] -> Usuario
crearUsuario elemento = Usuario (read (elemento !! 0) :: Integer) (elemento !! 1)

-- Acceso atributos

getCedula :: Usuario -> Cedula
getCedula (Usuario cedula _) = cedula

getNombreUsuario :: Usuario -> NombreUsuario
getNombreUsuario (Usuario _ nombre) = nombre


-- Muestra un usuario
-- E: un usuario
-- S: N/A
-- R: N/A
mostrarUser :: Usuario -> IO ()
mostrarUser usuario = do
    let
        cedula = getCedula usuario
        nombre = getNombreUsuario usuario
    putStrLn $ "Cédula: " ++ show cedula
    putStrLn $ "Nombre: " ++ nombre



-- Muestra todos los usuarios
-- Entrada: una lista de usuarios
-- Salida: N/A
-- Restricciones: N/A
mostrarAllUsuarios :: [Usuario] -> IO ()
mostrarAllUsuarios [] = return ()
mostrarAllUsuarios (usuario:restoUsuarios) = do
    mostrarUser usuario
    mostrarAllUsuarios restoUsuarios


-- Muestra la información de un usuario
-- Entrada: lista de usuarios, una cédula a verificar que exista
-- Salida: N/A
-- Restricciones: N/A
mostrarInfoUsuario :: [Usuario] -> Integer -> IO ()
mostrarInfoUsuario [] cedula = do
    putStrLn "\nLa cédula ingresada no se encuentra en el sistema.\n"
    return ()
mostrarInfoUsuario (usuario:restoUsuarios) cedula = do
    let cedulaUsuario = getCedula usuario
    if cedulaUsuario == cedula then do
        mostrarUser usuario
        putStrLn "Alquileres:\n"
    else
        mostrarInfoUsuario restoUsuarios cedula

-- Uso de map
-- Crea una lista de usuarios a partir de una lista de listas de strings
-- Entrada: una lista de listas de strings, donde cada lista interna representa la información de un usuario
-- Salida: una lista de usuarios
crearListaUsuarios :: [[String]] -> [Usuario]
crearListaUsuarios = map crearUsuario

--Crea una lista de usuarios
--E: una lista con listas de strings
--S: una lista de usuarios
separaUsuarios :: [[Char]] -> [Usuario]
separaUsuarios lista =
    if null lista then []
    else
        crearUsuario (separaPorComas (head lista, "")) : separaUsuarios (tail lista)


--lee un archivo de usuarios
--E: la ruta del archivo
--S: retorna una lista de usuarios
leerArchivoUsuarios :: FilePath -> IO [Usuario]
leerArchivoUsuarios archivo = do
    contenido <- readFile archivo
    let usuarios = separaUsuarios (lines contenido)
    return usuarios


cargaUsuario :: IO ()
cargaUsuario = do
    putStrLn "\n\nLeyendo usuarios desde el archivo usuarios.txt..."
    usuarios <- leerArchivoUsuarios "../Data App/usuarios.txt"
    putStrLn "Usuarios cargados:\n"
    mostrarAllUsuarios usuarios