module Facturar where

import System.IO
import FuncionesRelevantes


-- Se debe hacer import de alquilar y la carga de parqueos
-- Se va a suponer que estan

-- Estructura Factura
type IDFactura = Integer
type PuntoSalida = String
type PuntoDestino = String
type CedulaUsuarioFactura = Integer
type CodigoBicicletaFactura = String
type TipoBicicletaFactura = String
type DistanciaRecorrida = Float
type TarifaPorKilometro = Float
type MontoTotalFactura = Float

data Factura = Factura IDFactura PuntoSalida PuntoDestino CedulaUsuarioFactura CodigoBicicletaFactura TipoBicicletaFactura DistanciaRecorrida TarifaPorKilometro MontoTotalFactura



-- Constructor Factura
crearFactura :: [String] -> Factura
crearFactura elemento = Factura
    (read (elemento !! 0) :: IDFactura)
    (elemento !! 1)
    (elemento !! 2)
    (read (elemento !! 3) :: CedulaUsuarioFactura)
    (elemento !! 4)
    (elemento !! 5)
    (read (elemento !! 6) :: DistanciaRecorrida)
    (read (elemento !! 7) :: TarifaPorKilometro)
    (read (elemento !! 8) :: MontoTotalFactura)


-- Funciones get

getIdentificadorFactura :: Factura -> IDFactura
getIdentificadorFactura (Factura id _ _ _ _ _ _ _ _) = id

getPuntoSalida :: Factura -> PuntoSalida
getPuntoSalida (Factura _ pS _ _ _ _ _ _ _) = pS

getPuntoDestino :: Factura -> PuntoDestino
getPuntoDestino (Factura _ _ pD _ _ _ _ _ _) = pD

getCedulaUsuarioFactura :: Factura -> CedulaUsuarioFactura
getCedulaUsuarioFactura (Factura _ _ _ cF _ _ _ _ _) = cF

getCodigoBicicletaFactura :: Factura -> CodigoBicicletaFactura
getCodigoBicicletaFactura (Factura _ _ _ _ cBF _ _ _ _) = cBF

getTipoBicicletaFactura :: Factura -> TipoBicicletaFactura
getTipoBicicletaFactura (Factura _ _ _ _ _ tBF _ _ _) = tBF

getDistanciaRecorrida :: Factura -> DistanciaRecorrida
getDistanciaRecorrida (Factura _ _ _ _ _ _ k _ _) = k

getTarifaPorKilometro :: Factura -> TarifaPorKilometro
getTarifaPorKilometro (Factura _ _ _ _ _ _ _ t _) = t

getMontoTotalFactura :: Factura -> MontoTotalFactura
getMontoTotalFactura (Factura _ _ _ _ _ _ _ _ mT) = mT


-- Muestra la información de una factura por su ID
-- Entrada: lista de facturas, el ID de la factura a mostrar
-- Salida: N/A
mostrarFacturaPorID :: [Factura] -> IDFactura -> IO ()
mostrarFacturaPorID [] codigoFactura = putStrLn "El código de factura ingresado no existe"
mostrarFacturaPorID (factura:facturas) codigoFactura
    | getIdentificadorFactura factura == codigoFactura = mostrarDetalleFactura factura
    | otherwise = mostrarFacturaPorID facturas codigoFactura

-- Función auxiliar para mostrar una factura
mostrarDetalleFactura :: Factura -> IO ()
mostrarDetalleFactura factura = do
    let codigoFactura = getIdentificadorFactura factura
        puntoSalida = getPuntoSalida factura
        puntoDestino = getPuntoDestino factura
        cedulaUsuario = getCedulaUsuarioFactura factura
        codigoBicicleta = getCodigoBicicletaFactura factura
        tipoBicicleta = getTipoBicicletaFactura factura
        distanciaRecorrida = getDistanciaRecorrida factura
        tarifaPorKilometro = getTarifaPorKilometro factura
        montoTotalFactura = getMontoTotalFactura factura

    putStrLn "Información de la factura:"
    putStrLn $ "ID de Factura: " ++ show codigoFactura
    putStrLn $ "Punto de Salida: " ++ puntoSalida
    putStrLn $ "Punto de Destino: " ++ puntoDestino
    putStrLn $ "Cédula de Usuario: " ++ show cedulaUsuario
    putStrLn $ "Código de Bicicleta: " ++ codigoBicicleta
    putStrLn $ "Tipo de Bicicleta: " ++ tipoBicicleta
    putStrLn $ "Distancia Recorrida (en kilómetros): " ++ show distanciaRecorrida
    putStrLn $ "Tarifa por Kilómetro: " ++ show tarifaPorKilometro
    putStrLn $ "Monto Total de la Factura: " ++ show montoTotalFactura
    putStrLn "--------------------------------------"




-- NOTA IMPORTANTE
-- Esta funcionalidad requiere alquilar y parqueo, si bien una 
-- ya está que es parqueo, la otra presenta errores (ya identificados), hasta confirmar que alquilar esté bien
-- esto permanecerá comentado


-- Crea una nueva factura y la agrega a la lista de facturas
-- Entrada: lista de alquileres, ID de la factura, ID del alquiler a facturar, tarifa eléctrica, tarifa a pedal, lista de parqueos
-- Salida: lista de facturas con la nueva factura ya agregada
--crearNuevaFactura :: [Alquiler] -> Int -> Integer -> Float -> Float -> [Parqueo] -> IO [Factura]
--crearNuevaFactura [] _ _ _ _ _ = do
--    putStrLn "\nEl código de alquiler que ingresó no existe.\n"
--    return []
--crearNuevaFactura alquileres idFactura idAlquiler tarifaElectrica tarifaPedal parqueos = do
--    let codAlquiler = getIdentificador (head alquileres)

--    if idAlquiler == codAlquiler then do
--        let estado = getEstado (head alquileres)
--        if estado == "facturado" then do
--            putStrLn "\nEl código de alquiler que ingresó ya se encuentra facturado.\n"
--            return []
--        else do
--            let tipoBici = getTipoBici (head alquileres)
            -- Determinar la tarifa adecuada según el tipo de bicicleta
--            let tarifa = if tipoBici == "AE" then tarifaElectrica else tarifaPedal
--            return [crearNuevaFacturaAux (head alquileres) tarifa idFactura parqueos]
--    else
--        crearNuevaFactura (tail alquileres) idFactura idAlquiler tarifaElectrica tarifaPedal parqueos

-- Crea la nueva factura
-- Entrada: el alquiler a facturar, tarifa correspondiente al tipo de bicicleta, ID de la factura, lista de parqueos
-- Salida: la factura nueva
--crearNuevaFacturaAux :: Alquiler -> Float -> Int -> [Parqueo] -> Factura
--crearNuevaFacturaAux alquiler tarifa idFactura parqueos = do
--    let idAlquiler = getIdentificador alquiler
--    let nombreParqueoLlegada = getParqueoLlegada alquiler
--    let nombreParqueoSalida = getParqueoSalida alquiler
--    let parqueoLlegada = getParqueo nombreParqueoLlegada parqueos
--    let parqueoSalida = getParqueo nombreParqueoSalida parqueos
--    let cedulaUsuario = getCedulaUsuario alquiler
--    let codigoBicicleta = getCodigoBici alquiler
--    let tipoBicicleta = getTipoBici alquiler
--    let ubicacionXParqueoLlegada = getUbicacionX parqueoLlegada
--    let ubicacionYParqueoLlegada = getUbicacionY parqueoLlegada
--    let ubicacionXParqueoSalida = getUbicacionX parqueoSalida
--    let ubicacionYParqueoSalida = getUbicacionY parqueoSalida
--    let distanciaRecorrida = obtenerDistancia ubicacionXParqueoLlegada ubicacionYParqueoLlegada ubicacionXParqueoSalida ubicacionYParqueoSalida
--    let montoTotal = tarifa * distanciaRecorrida
--    crearFactura [show idAlquiler, nombreParqueoSalida, nombreParqueoLlegada, show cedulaUsuario, codigoBicicleta, tipoBicicleta, show distanciaRecorrida, show tarifa, show montoTotal]

--  


-- fin de la funcionalidad que requiere alquilar




-- Muestra todas las facturas en la lista
-- Entrada: una lista de facturas
-- Salida: N/A
mostrarTodasLasFacturas :: [Factura] -> IO ()
mostrarTodasLasFacturas [] = return ()
mostrarTodasLasFacturas (factura:facturas) = do
    mostrarDetalleFactura factura
    mostrarTodasLasFacturas facturas



-- Muestra una sola factura al usuario
-- Entrada: lista de facturas, código de la factura a mostrar, nombre de la empresa, sitio web de la empresa, contacto de la empresa
-- Salida: N/A
mostrarUnaFactura :: [Factura] -> Integer -> String -> String -> String -> IO ()
mostrarUnaFactura [] _ _ _ _ = do
    putStrLn "\nEl código que ingresó no se encuentra en el sistema.\n"
    return ()
mostrarUnaFactura (factura:facturas) codigo nombreEmpresa sitioWeb contacto
    | getIdentificadorFactura factura == codigo = do
        putStrLn "--------------------------------------"
        putStrLn $ "Empresa: " ++ nombreEmpresa
        putStrLn $ "Sitio Web: " ++ sitioWeb
        putStrLn $ "Contacto: " ++ contacto
        mostrarDetalleFactura factura
    | otherwise = mostrarUnaFactura facturas codigo nombreEmpresa sitioWeb contacto


-- Convierte una lista de cadenas de texto con información de facturas en una lista de facturas
-- Entrada: una lista de cadenas de texto donde cada cadena representa la información de una factura
-- Salida: una lista de facturas
convertirCadenasAFacturas :: [[Char]] -> [Factura]
convertirCadenasAFacturas = map
      (\ cadena -> crearFactura (separaPorComas (cadena, "")))


-- Lee el archivo de facturas
-- Entrada: ruta del archivo
-- Salida: retorna una lista de facturas
leerArchivoFacturas :: FilePath -> IO [Factura]
leerArchivoFacturas archivo = do
    contenido <- readFile archivo
    let facturas = convertirCadenasAFacturas (lines contenido)
    return facturas


-- Agrega una factura al archivo de facturas
-- Entrada: la factura a agregar
-- Salida: N/A
guardarFacturaEnArchivo :: Factura -> IO ()
guardarFacturaEnArchivo factura = do
    let idFactura = getIdentificadorFactura factura
    let puntoSalida = getPuntoSalida factura
    let puntoDestino = getPuntoDestino factura
    let cedulaUsuario = getCedulaUsuarioFactura factura
    let codigoBicicleta = getCodigoBicicletaFactura factura
    let tipoBicicleta = getTipoBicicletaFactura factura
    let distanciaRecorrida = getDistanciaRecorrida factura
    let tarifaPorKilometro = getTarifaPorKilometro factura
    let montoTotal = getMontoTotalFactura factura
    let facturaString = show idFactura ++ "," ++ puntoSalida ++ "," ++ puntoDestino ++ "," ++ show cedulaUsuario ++ "," ++ codigoBicicleta ++ "," ++ tipoBicicleta ++ "," ++ show distanciaRecorrida ++ "," ++ show tarifaPorKilometro ++ "," ++ show montoTotal ++ "\n"
    appendFile "facturas.txt" facturaString
