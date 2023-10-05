module Main2 where

import Facturar

main :: IO ()
main = do
    -- Carga las facturas desde el archivo
    facturas <- leerArchivoFacturas "facturas.txt"

    -- Muestra todas las facturas
    putStrLn "Todas las facturas:"
    mostrarTodasLasFacturas facturas

    -- Muestra una factura específica por su ID
    putStrLn "Ingrese el ID de la factura que desea consultar:"
    idFactura <- readLn
    mostrarFacturaPorID facturas idFactura

    -- También puedes agregar una nueva factura y guardarla en el archivo
    -- Esto dependerá de cómo implementes la funcionalidad de alquilar y parqueo

    putStrLn "¡Listo!"
