import Text.Show.Functions ()
import Data.Char (toUpper, isUpper)

type Habilidad = String
type Objeto = Barbaro -> Barbaro

data Barbaro = Barbaro{
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
}deriving Show

-- PUNTO 1

dave = Barbaro "Davo" 100 ["Fiumba", "Porcino"] []
juan = Barbaro "Juan" 100 ["Pelear", "Porcino","Robar"] []

espada :: Int -> Objeto
espada peso unBarbaro = modificarFuerza (2 * peso) unBarbaro

modificarFuerza :: Int -> Barbaro -> Barbaro
modificarFuerza cantidad unBarbaro = unBarbaro{fuerza = fuerza unBarbaro + cantidad}

amuletoMistico :: String -> Objeto
amuletoMistico habilidad unBarbaro = agregarHabilidad habilidad unBarbaro

agregarHabilidad :: String -> Barbaro -> Barbaro
agregarHabilidad unaHabilidad unBarbaro = unBarbaro{habilidades = unaHabilidad : habilidades unBarbaro}

varitaDefectuosa :: Objeto
varitaDefectuosa unBarbaro = agregarHabilidad "magia" . quitarObjetos $ unBarbaro

quitarObjetos :: Barbaro -> Barbaro
quitarObjetos unBarbaro = unBarbaro{objetos = []}

ardilla :: Objeto
ardilla = id

cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto1 objeto2 unBarbaro =  objeto2 . objeto1 $ unBarbaro

-- PUNTO 2

megafono :: Objeto
megafono unBarbaro = amplificar unBarbaro

amplificar :: Barbaro -> Barbaro
amplificar unBarbaro = unBarbaro{habilidades =  [map (toUpper) (concat (habilidades unBarbaro))]}

megafonoBarbarico ::  Objeto
megafonoBarbarico unBarbaro = cuerda ardilla megafono unBarbaro

-- PUNTO 3

type Aventura = [Evento]
type Evento = Barbaro -> Bool
type Prueba = Barbaro -> Bool

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes = tiene "Escribir Poesía Atroz"
tiene :: String -> Barbaro -> Bool
tiene habilidad unBarbaro = elem habilidad . habilidades $ unBarbaro

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro =  nombre unBarbaro == "Faffy" || nombre unBarbaro == "Astro"
-- punto 3.3
ritualDeFechorias :: [Prueba] -> Evento
ritualDeFechorias pruebas unBarbaro = any (\prueba -> prueba unBarbaro) pruebas
--ritualDeFechorias' :: [Prueba] -> Evento
--ritualDeFechorias' pruebas unBarbaro = any (\prueba -> prueba unBarbaro) pruebas
--hacerPrueba :: Prueba -> Barbaro -> Bool
--hacerPrueba prueba unBarbaro = prueba unBarbaro

saqueo :: Prueba
saqueo unBarbaro =  tiene "robar" unBarbaro && fuerza unBarbaro > 80

gritoDeGuerra :: Prueba
gritoDeGuerra unBarbaro = obtenerPoder unBarbaro >= 4 * length (objetos unBarbaro)

obtenerPoder :: Barbaro -> Int
obtenerPoder unBarbaro = length . concat . habilidades $ unBarbaro
--

caligrafia :: Prueba
caligrafia unBarbaro = obtenerVocales unBarbaro > 3 && habilidadesConMayuscula unBarbaro

habilidadesConMayuscula :: Barbaro -> Bool
habilidadesConMayuscula unBarbaro = all isUpper . map obtenerPrimerLetra . habilidades $ unBarbaro

obtenerPrimerLetra :: Habilidad -> Char
obtenerPrimerLetra habilidad = head habilidad

obtenerVocales :: Barbaro -> Int
obtenerVocales unBarbaro = length . filter esVocal . concat . habilidades . amplificar $ unBarbaro

esVocal :: Char -> Bool
esVocal char = elem char "aeiouAEIOU" 

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes barbaros aventura = filter (sobreviveAAventura aventura) barbaros

sobreviveAAventura ::  Aventura -> Barbaro -> Bool
sobreviveAAventura aventura unBarbaro = any (sobreviveAEvento unBarbaro) aventura

sobreviveAEvento :: Barbaro -> Evento -> Bool
sobreviveAEvento unBarbaro evento = evento unBarbaro
