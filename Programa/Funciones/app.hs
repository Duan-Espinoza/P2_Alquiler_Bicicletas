---Importaciones librerias para  Paqueo
import System.Directory  --Libreria para verificacion de rutas
import Data.List (isInfixOf) --Libreria para validacion de las provincias 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (elem)
import Control.Exception (try)
import GHC.Read (readField)
import CargaParqueos
import LoadUsuarios
import Alquilar 
import CargaMuestraBike  -- Importa tu módulo principal aquí
import Estadisticas 
import ConsultaBike
import Facturar 
-- Seccion de vistas 
vistaMenuInicial = do
    putStrLn "\n\n \tBienvenido"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"

vistaOperativas = do 
    putStrLn "\n\n \tSe encuentra en opciones Operativas"
    putStrLn "1.Carga y Mostrar parqueos"
    putStrLn "2.Mostrar y asignar bicicletas"
    putStrLn "3.Cargar Usuarios"
    putStrLn "4. Estadísticas"
    putStrLn "5.Volver"

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
                --CONSULTAR BICICLETA
                putStrLn "\n\nIndique la ubicacion del archivo de Parqueos:"
                ruta <- getLine
                putStrLn "\n\nNOTA: coloque la siguiente informacion una por una"
                ConsultaBike.mainConsulta ruta
                putStrLn "\n\n"
                vistaGenerales
                menuGenerales
            
        else if op == 2 
            then do 
                -- ALQUILAR
                Alquilar.alquila
                putStrLn "\n\n"
                menuGenerales
        else if op == 3
            then do 
                --FACTURAR
                putStrLn "\n\nBienvenido a Facturar"
                putStrLn "1.Mostrar Todas facturas"
                putStrLn "2.Mostrar facturas por ID"
                putStrLn "3.Volver"
                putStrLn "Seleccione una opcion: "
                facturas <- leerArchivoFacturas "../Data App/facturas.txt"
                opcion <- getLine
                case opcion of
                    "1" -> do 
                        Facturar.mostrarTodasLasFacturas facturas
                        putStrLn "\n\n"
                        vistaGenerales
                        menuGenerales
                    "2" -> do
                        putStrLn "Para Mostrar una Factura indique elo identificador de factura: "
                        idFactura <- readLn
                        Facturar.mostrarFacturaPorID facturas idFactura
                        putStrLn "\n\n"
                        vistaGenerales
                        menuGenerales
                    _   -> do 
                        vistaGenerales
                        menuGenerales 
                
        else if op == 4
            then do 
                --VOLVER
                main
        else 
            do
                putStrLn "Debe de selecccionar una de las opciones mostradas"
                vistaGenerales
                menuGenerales
esPrueba = do 
    CargaParqueos.consultaRuta

menuOperativas = do 
    putStrLn "\nIndique una de las opciones mostradas: "
    opcion <- getLine
    let op = (read opcion :: Int)
    if null opcion then do 
        putStrLn "\n -----  Debe de colocar una de las opciones mostradas  -----"
        vistaOperativas
        menuOperativas
    else
        if op == 1 
            then  do
                -- CARGA Y MUESTRA DE PARQUEOS
                CargaParqueos.consultaRuta
                vistaOperativas
                menuOperativas 
        else if op == 2
            then do 
                -- MOSTRAR Y ASIGNAR BICIS
                putStrLn "\n\nBienvenido al programa de prueba de CargaMuestraBike."
                putStrLn "Indique la ruta para Carga Bicicletas" -- Falta validacion para que sea existente
                baseDatosBicis <- getLine
                putStrLn "\n\nSe cargo con exito. Desea Hacer consulta?"
                putStrLn "1.Si"
                putStrLn "2.No"
                opcion <- getLine
                case opcion of
                    "1" -> do 
                        muestraActionsBicis baseDatosBicis
                        putStrLn "\n\n"
                        vistaOperativas
                        menuOperativas
                    "2" -> do
                        vistaOperativas
                        menuOperativas
                    _   -> do
                        vistaOperativas
                        menuOperativas
                     
        else if op == 3
            then do 
                -- CARGA USUARIO
                putStrLn "\n Indique la ruta para cargar los usuarios"
                ruta <- getLine
                LoadUsuarios.cargaUsuario
                putStrLn "\n\n"
                vistaOperativas
                menuOperativas

        else if op == 4
            then do 
                -- ESTADISTICAS
                putStrLn "\n\nBienvenido a Estadisticas"
                putStrLn "1.Top 5 Bicicletas con mas viajes ."
                putStrLn "2.Top 5 Parqueos con mas viajes."
                putStrLn "4.Volver"
                putStrLn "Seleccione una de las opciones mostradas"
                opcion <- getLine
                case opcion of
                    "1" -> do 
                        Estadisticas.topParqueo
                        putStrLn "\n\n"
                        vistaOperativas
                        menuOperativas
                    "2" -> do
                        Estadisticas.topBici
                        putStrLn "\n\n"
                        vistaOperativas
                        menuOperativas
                    _   -> do
                        vistaOperativas
                        menuOperativas
        else if op == 5
            then do 
                -- VOLVER
                main
        else 
            do
                putStrLn "\n\n  ---------   Debe de selecccionar una de las opciones mostradas    --------"
                vistaOperativas
                menuOperativas


-- Funcion valida asignacion de parqueos
asignaParqueo ruta= do 
    let baseNew = leerArchivoBicicletas ruta
    putStrLn "\n\nSe Cargo con exito a la base de datos" 


--funcion encargada de permitir mostrar bicis en base a lo que diga el usuario
muestraActionsBicis baseDatosBicis = do 
    putStrLn "\n\nIndique el nombre del parqueo donde desea hacer la consulta"
    parqueoSolicitud <- getLine 

    let baseDatosParqueos = "../Data App/infoParqueos.txt" --Datos almacenados
    --let baseDatosBicis = "../Data App/bicicletas.txt"

    baseParqueos <- CargaParqueos.leerArchivo baseDatosParqueos
    baseBicis <- CargaMuestraBike.leerArchivoBicis baseDatosBicis
    
    if parqueoSolicitud == "#" 
        then do
            putStrLn "\n\n\nMostrando todas las bicicletas:  \n" 
            mostrarBicicletas baseBicis
        else do
            if parqueoSolicitud == "transito"
                then do 
                    putStrLn "\n\n\nMostrando bicicletas en tránsito:"
                    mostrarBicicletasEnParqueo baseBicis "transito"
                else do
                    if CargaParqueos.existeNombreAux baseParqueos parqueoSolicitud
                        then do
                            putStrLn "\n\nMostrando bicicletas del parqueo" 
                            CargaMuestraBike.mostrarBicicletasEnParqueo baseBicis parqueoSolicitud


                        else putStrLn "Debe de indicar parqueos registrados en el sistema xd"
                
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

