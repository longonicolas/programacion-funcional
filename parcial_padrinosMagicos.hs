import Text.Show.Functions()

data Chico = Chico{

    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Deseo]
}deriving Show

type Deseo = Chico -> Chico

chico1 :: Chico
chico1 = Chico "Juan" 15 ["correr"] []
chico2 :: Chico
chico2 = Chico "Pedro" 15 ["correr", "bailar"] [serGrosoEnNFS, aprenderHabilidades ["nadar"]]
chico3 :: Chico
chico3 = Chico "Elias" 15 ["correr","cocinar"] [serGrosoEnNFS, aprenderHabilidades ["bailar"]]

pibardos :: [Chico]
pibardos = [chico1,chico2, chico3]

modificarHabilidades :: ([String] -> [String]) -> Chico -> Chico
modificarHabilidades funcion chico = chico{habilidades = funcion . habilidades $ chico}

modificarEdad :: (Int -> Int) -> Chico -> Chico
modificarEdad funcion chico = chico{edad = funcion . edad $ chico}

todasLasVersionesDeNFSpeed :: [String]
todasLasVersionesDeNFSpeed = map (\num -> "jugar need for speed " ++ show num) [1 ..]

aprenderHabilidades :: [String] -> Deseo
aprenderHabilidades nuevasHabilidades = modificarHabilidades (nuevasHabilidades ++)

serGrosoEnNFS :: Deseo
serGrosoEnNFS = aprenderHabilidades todasLasVersionesDeNFSpeed

serMayor :: Deseo
serMayor = modificarEdad (const 18)

wanda :: Chico -> Chico
wanda unChico = modificarEdad (+1) . cumplirDeseos (take 1 $ deseos unChico) $ unChico

cosmo :: Chico -> Chico
cosmo unChico = modificarEdad (const $ div (edad unChico) 2) unChico

muffinMagico :: Chico -> Chico
muffinMagico unChico = cumplirDeseos (deseos unChico) $ unChico

cumplirDeseos :: [Deseo] -> Chico -> Chico
cumplirDeseos deseos unChico = foldr ($) unChico deseos

cumplirTodosLosDeseos :: [[Deseo]] -> [Chico] -> [Chico]
cumplirTodosLosDeseos lista chicos = zipWith cumplirDeseos lista chicos

-- PARTE B

data Chica = Chica{

    nombreChica :: String,
    condicion :: Condicion
}deriving Show


type Condicion = Chico -> Bool

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica pretendientes
        | any (condicion unaChica) pretendientes = head (pretendientesAptos (condicion unaChica) pretendientes)
        | otherwise = last pretendientes

pretendientesAptos :: Condicion -> [Chico] -> [Chico]
pretendientesAptos condicion pretendientes = filter condicion pretendientes

tieneHabilidad :: String -> Chico -> Bool
tieneHabilidad habilidad chico = elem habilidad . habilidades $ chico

esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = (edad unChico) > 18 && tieneHabilidad "manejar" unChico

nuevaChica :: Chica
nuevaChica = Chica "Juana" (tieneHabilidad "cocinar")

-- Parte C

habilidadesProhibidas :: [String]
habilidadesProhibidas = ["enamorar","matar","dominar el mundo"]

infractoresDaRules :: [Chico] -> [String]
infractoresDaRules chicos = map nombre . filter deseosProhibidos . cumplirTodosLosDeseos (map deseos $ chicos) $ chicos

deseosProhibidos :: Chico -> Bool
deseosProhibidos = any (esHabilidadProhibida) . habilidades

esHabilidadProhibida :: String -> Bool
esHabilidadProhibida unaHabilidad = elem unaHabilidad habilidadesProhibidas
