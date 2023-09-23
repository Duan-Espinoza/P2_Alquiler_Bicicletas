




-- Datos de la empresa
nombreEmpresa = "Bicicletas Alquiler Brenes y Asociados"
sitioWeb = "bicisbrenesasoc.com"
contacto = "25554888"
tarifaPedal = 400
tarifaElectrico = 700


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
                putStrLn "\nEn DESAROLSDLO "
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


 