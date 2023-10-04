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
