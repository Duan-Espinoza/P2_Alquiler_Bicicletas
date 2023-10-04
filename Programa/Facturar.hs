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
