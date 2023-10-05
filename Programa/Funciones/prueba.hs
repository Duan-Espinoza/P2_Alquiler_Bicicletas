type IdParqueo = Integer
type NombreParqueo = String
type DireccionParqueo = String
type ProvinciaParqueo = String
type CordenadaX = Float
type CordenadaY = Float

data Parqueo = Parqueo IdParqueo NombreParqueo DireccionParqueo ProvinciaParqueo CordenadaX CordenadaY

creaParqueo :: [String] -> Parqueo
creaParqueo elemento = Parqueo (read (elemento !! 0) :: Integer) (elemento !! 1) (elemento !! 2) (elemento !! 3)(read (elemento !! 4) :: Float) (read (elemento !! 5) :: Float)

-- Metodos Accesores
getId :: Parqueo -> IdParqueo
getId (Parqueo id _ _ _ _ _) = id
getNombreParqueo :: Parqueo -> NombreParqueo
getNombreParqueo (Parqueo _ nombre _ _ _ _) = nombre
getDireccionParqueo :: Parqueo -> DireccionParqueo
getDireccionParqueo (Parqueo _ _ direccion _ _ _) = direccion
geProvincia :: Parqueo -> ProvinciaParqueo
geProvincia(Parqueo _ _ _ provincia _ _) = provincia
getCordenadaX :: Parqueo -> CordenadaX
getCordenadaX(Parqueo _ _ _ _ cordenada _ ) = cordenada
getCordenadY :: Parqueo -> CordenadaY
getCordenadY(Parqueo _ _ _ _ _ cordenadaY ) = cordenadaY

showParqueo parqueo = 
    let 
        id = getId(parqueo)
        nombre = getNombreParqueo(parqueo)
        direccion = getDireccionParqueo(parqueo)
        provincia = geProvincia(parqueo)
        cordenadaX = getCordenadaX(parqueo)
        cordenadaY = getCordenadY(parqueo)

    in
        "El id es: " ++ show id  ++ ", nombre del Parque: "++ nombre ++ ", su direccion es: " ++ direccion ++ ", provincia: " ++ provincia ++ ", coordenada X: "++ show cordenadaX ++ ", coordenada Y: " ++ show  cordenadaY

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

main :: IO ()
main = do
    putStrLn "Indique la ruta del archivo de parqueos:"
    filePath <- getLine
    parqueos <- leerArchivo filePath
    putStrLn "Parqueos cargados:"
    showParqueos parqueos
