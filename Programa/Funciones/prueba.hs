type IdParqueo = Integer
type NombreParqueo = String
type DireccionParqueo = String
type ProvinciaParqueo = String
type CordenadaX = Float
type CordenadaY = Float

data Parqueo = Parqueo IdParqueo NombreParqueo DireccionParqueo ProvinciaParqueo CordenadaX CordenadaY

creaParqueo :: [String] -> Parqueo
creaParqueo elemento = Parqueo (read (elemento !! 0) :: Integer) (elemento !! 1) (elemento !! 2) (elemento !! 3)(read (elemento !! 4) :: Float) (read (elemento !! 5) :: Float)

getId :: Parqueo -> IdParqueo
getId (Parqueo id _ _ _ _ _) = id

showParqueo parqueo = 
    let 
        id = getId(parqueo)
    in
        "Parqueo" ++ show id  

showParqueos [] = print("")
showParqueos lista = 
    do 
        print(showParqueo(head lista))
        showParqueos(tail lista)

existeID parqueos = 
    do 
        putStrLn ("Indique un id")
        tempId <- getLine
        let id = (read tempId :: Integer)
        existeIdAux(parqueos, id) 

existeIdAux ([],cedula) = print ("no encontrado")
existeParqueoAux(parqueos, id) = do 
    let primero = (head parqueos)
    let idNew = getId(primero)
    if idNew == id then 
        print "es"
    else
        existeParqueoAux((tail parqueos), id) 

separaPorComas :: ([Char], [Char]) -> [[Char]]
separaPorComas (cadena,temp) = --split
    if cadena == "" then[temp] else
        if (head cadena) == (head ",") then 
            [temp] ++ separaPorComas((tail cadena), "")
        else
            separaPorComas((tail cadena), temp++[(head cadena)])


separaElementos :: [[Char]] -> [Parqueo]
separaElementos lista = 
    if lista == [] then 
        []
    else 
        [creaParqueo(separaPorComas((head lista), ""))] ++ separaElementos(tail lista)

convierteALineas :: String -> [String]
convierteALineas texto = lines texto

leerArchivo :: FilePath -> IO [Parqueo]
leerArchivo archivo = do
    contenido <- readFile archivo
    let parqueos = separaElementos(convierteALineas contenido)
    return parqueos

menuAux(opcion, parqueos, persona2) = 
    if opcion == 6 then 
        print "bye"
    else
        case opcion of 
            1 -> showParqueos parqueos

menu = do
    putStrLn "indique ruta de parqueos 1"
    ruta <- getLine
    putStrLn  "Indique la ruta de parqueos 2"
    parqueos <- leerArchivo ruta
    ruta2 <- getLine
    parqueos2 <- leerArchivo ruta2
    temp <- menuAux (1,parqueos,parqueos2)
    return temp


