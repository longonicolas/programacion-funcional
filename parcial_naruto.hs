data Ninja = Ninja{

    nombre :: String,
    herramientas :: [Herramienta],
    rango :: Int
}deriving Show

data Herramienta = Herramienta{

    nombreHerramienta :: String,
    cantidad :: Int
}deriving Show

modificarHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
modificarHerramientas funcion unNinja = unNinja{herramientas = funcion . herramientas $ unNinja}

cantHerramientas :: Ninja -> Int
cantHerramientas unNinja = sum . map cantidad . herramientas $ unNinja

agregarHerramienta :: Herramienta -> Ninja -> Ninja
agregarHerramienta herramienta = modificarHerramientas (herramienta:)

quitarExcedente :: Herramienta -> Int -> Herramienta
quitarExcedente herramienta cant = herramienta{cantidad = 100 - cant}

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta herramienta unNinja
        | cantHerramientas unNinja + (cantidad herramienta) <= 100 = agregarHerramienta herramienta unNinja
        | otherwise = agregarHerramienta (quitarExcedente herramienta (cantHerramientas unNinja)) unNinja

encontrarHerramienta :: Herramienta -> Herramienta -> Bool
encontrarHerramienta herramienta1 herramienta2 = (nombreHerramienta herramienta1) == (nombreHerramienta herramienta2)

usarHerramienta :: Herramienta -> Ninja -> Ninja
usarHerramienta herramienta unNinja = modificarHerramientas (filter $ not. encontrarHerramienta herramienta) unNinja