module Main where

import CargaMuestraBike  -- Importa tu módulo principal aquí

-- Función para realizar pruebas
realizarPruebas :: IO ()
realizarPruebas = do
    putStrLn "Realizando pruebas..."

    -- Ejemplo de cómo realizar pruebas:
    let bicicletas = [ crearBicicleta "B001" "TR" "Parqueo1"
                     , crearBicicleta "B002" "AE" "Parqueo2"
                     , crearBicicleta "B003" "AG" "Parqueo3"
                     ]
    
    putStrLn "Mostrando todas las bicicletas:"
    mostrarBicicletas bicicletas

    putStrLn "Mostrando bicicletas en Parqueo1:"
    mostrarBicicletasEnParqueo bicicletas "Parqueo1"

    putStrLn "Mostrando bicicletas en tránsito:"
    mostrarBicicletasEnParqueo bicicletas "transito"

    putStrLn "Pruebas completadas."

-- Función principal
main :: IO ()
main = do
    putStrLn "Bienvenido al programa de prueba de CargaMuestraBike."
    putStrLn "Presiona '1' para realizar pruebas."
    putStrLn "Presiona '2' para salir."
    
    opcion <- getLine
    case opcion of
        "1" -> realizarPruebas
        "2" -> putStrLn "Saliendo del programa."
        _   -> putStrLn "Opción no válida."
