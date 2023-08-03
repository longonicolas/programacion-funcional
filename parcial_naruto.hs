import Text.Show.Functions()

data Ninja = Ninja{

    nombre :: String,
    herramientas :: [Herramienta],
    jutsus :: [Jutsu],
    rango :: Int
}deriving Show

data Herramienta = Herramienta{

    nombreHerramienta :: String,
    cantidad :: Int
}deriving (Show,Eq)

ninja1 = Ninja "Juan" [] [clonesDeSombra 5, fuerzaDeUnCentenar] 5
ninja2 = Ninja "Pedro" [] [] 8
ninja3 = Ninja "Julian" [] [] 600
ninja4 = Ninja "Eze" [] [] 400

losNinjas = [ninja1,ninja2,ninja3,ninja4]

laMision = Mision 2 8 [ninja4,ninja3] kunai

-- PARTE A

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

-- PARTE B

data Mision = Mision{

    cantNinjas :: Int,
    rangoNinjas :: Int,
    enemigos :: [Ninja],
    recompensa :: Herramienta
}deriving Show

esDesafiante :: [Ninja] -> Mision -> Bool
esDesafiante ninjas mision = minimum (rangosDe ninjas) < rangoNinjas mision && cantidadEnemigos mision >= 2

cantidadEnemigos :: Mision -> Int
cantidadEnemigos mision =  length . enemigos $ mision

rangosDe :: [Ninja] -> [Int]
rangosDe ninjas =  map rango $ ninjas

kunai :: Herramienta
kunai = Herramienta "Kunai" 14
shuriken :: Herramienta
shuriken = Herramienta "Shuriken" 5
bHumo :: Herramienta
bHumo = Herramienta "BHumo" 3

herramientasCopadas :: [Herramienta]
herramientasCopadas = [kunai,shuriken,bHumo]

esCopada :: Mision -> Bool
esCopada unaMision = elem (recompensa unaMision) herramientasCopadas

sumaTotalDeHerramientas :: [Ninja] -> Int
sumaTotalDeHerramientas ninjas = sum . map cantHerramientas $ ninjas

esFactible :: [Ninja] -> Mision -> Bool
esFactible ninjas unaMision = not (esDesafiante ninjas unaMision) && (length ninjas >= cantNinjas unaMision || sumaTotalDeHerramientas ninjas > 500)

tieneRangoRecomendado :: Int -> Ninja -> Bool
tieneRangoRecomendado unRango ninja = rango ninja >= unRango

modificarRango :: (Int -> Int) -> Ninja -> Ninja
modificarRango funcion ninja = ninja{rango = max 0 . funcion . rango $ ninja}

fallarMision :: Mision -> [Ninja] -> [Ninja]
fallarMision mision ninjas = map (modificarRango (subtract 2)) $ (filter (tieneRangoRecomendado (rangoNinjas mision)) $ ninjas)

cumplirMision :: Mision -> [Ninja] -> [Ninja]
cumplirMision mision ninjas = map (modificarRango (+1)) $ (map (obtenerHerramienta $ recompensa mision) $ ninjas)

cumplirMision' :: Mision -> [Ninja] -> [Ninja]
cumplirMision' mision = map (modificarRango (+1) . obtenerHerramienta (recompensa mision))

type Jutsu = Mision -> Mision

modificarCantNinjas :: (Int -> Int) -> Mision -> Mision
modificarCantNinjas funcion mision = mision{cantNinjas = max 1 . funcion . cantNinjas $ mision}

modificarEnemigosMision :: ([Ninja] -> [Ninja]) -> Mision -> Mision
modificarEnemigosMision funcion mision = mision{enemigos = funcion . enemigos $ mision}

clonesDeSombra :: Int -> Jutsu
clonesDeSombra clones = modificarCantNinjas (subtract clones)

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar = modificarEnemigosMision (filter (tieneRangoRecomendado 500))

aplicarJutsus :: Ninja -> Mision -> Mision
aplicarJutsus ninja mision = foldr ($) mision (jutsus ninja)

aplicarTodosLosJutsus :: [Ninja] -> Mision -> Mision
aplicarTodosLosJutsus ninjas mision = foldr aplicarJutsus mision ninjas

misionFactibleOCopada :: [Ninja] -> Mision -> Bool
misionFactibleOCopada ninjas unaMision = esCopada unaMision || esFactible ninjas unaMision

ejecutarMision :: Mision -> [Ninja] -> [Ninja]
ejecutarMision mision ninjas
        | misionFactibleOCopada ninjas (aplicarTodosLosJutsus ninjas mision) = cumplirMision mision ninjas
        | otherwise = fallarMision mision ninjas

-- PARTE C

abanico :: Herramienta
abanico = Herramienta "Abanico" 1

guerraNinja :: Mision
guerraNinja = Mision 100000 100 ninjasInfinitos abanico

ninjasInfinitos :: [Ninja]
ninjasInfinitos = map (\x -> Ninja ("Zetsu " ++ show x) [] [] 600) [1..]


{- 
esDesafiante : no puede responder ya que se quedará buscando el minimo de un elemento cuya cantidad es infinita, no puede 
aplicar lazy evaluation
esCopada : responde correctamente (False) ya que a pesar de tener infinitos enemigos, tiene una sola herramienta y no es copada
fuerzaDeUnCentenar : muestra la mision tal como la mostraba antes. Si la fuerza de los ninjas fuera menor a 500 no podria responder

-}