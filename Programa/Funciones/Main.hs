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
                     , crearBicicleta "B004" "TR" "Parqueo4"
                     , crearBicicleta "B005" "AE" "Parqueo5"
                     , crearBicicleta "B006" "AG" "Parqueo6"
                     , crearBicicleta "B007" "TR" "transito"
                     , crearBicicleta "B007" "TR" "Parqueo7"
                     , crearBicicleta "B008" "AE" "Parqueo8"
                     , crearBicicleta "B009" "AG" "Parqueo9"
                     , crearBicicleta "B0010" "TR" "Parqueo10"
                     , crearBicicleta "B0011" "AE" "Parqueo11"
                     , crearBicicleta "B0012" "AG" "Parqueo12"
                     , crearBicicleta "B0013" "TR" "Parqueo13"
                     , crearBicicleta "B0014" "AE" "Parqueo14"
                     , crearBicicleta "B0015" "AG" "Parqueo15"
                     ]
    
 

    putStrLn "Mostrando bicicletas en Parqueo1:"
    mostrarBicicletasEnParqueo bicicletas "Parqueo7"

    

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
