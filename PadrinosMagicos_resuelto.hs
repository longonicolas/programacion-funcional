import Text.Show.Functions

---------- Funciones Auxiliares ----------

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Chico -> Chico
mapHabilidades unaFuncion unChico = unChico { habilidades = unaFuncion . habilidades $ unChico }

mapEdad :: (Int -> Int) -> Chico -> Chico
mapEdad unaFuncion unChico = unChico { edad = unaFuncion . edad $ unChico }

setEdad :: Int -> Chico -> Chico
setEdad = mapEdad . const

---------- Funciones Auxiliares ----------

-- Punto A

timmy :: Chico
timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

-- Punto A.1

data Chico = Chico {
  nombre      :: String,
  edad        :: Int,
  habilidades :: [Habilidad],
  deseos      :: [Deseo]
} deriving Show

type Habilidad = String
type Deseo     = Chico -> Chico

-- Punto A.1.a

aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades habilidades = mapHabilidades (++ habilidades)

-- Punto A.1.b

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed = aprenderHabilidades habilidadesDeJugarAlNeedForSpeed

habilidadesDeJugarAlNeedForSpeed :: [Habilidad]
habilidadesDeJugarAlNeedForSpeed = map (("jugar need for speed " ++) . show) [1..]

-- Punto A.1.c

serMayor :: Deseo
serMayor = setEdad 18

-- Punto A.2

type Padrino = Chico -> Chico

-- Punto A.2.a

wanda :: Padrino
wanda = madurar . cumplirPrimerDeseo

cumplirPrimerDeseo :: Chico -> Chico
cumplirPrimerDeseo unChico = (head . deseos) unChico unChico

madurar :: Chico -> Chico
madurar = mapEdad succ

-- Punto A.2.a

cosmo :: Padrino
cosmo = mapEdad (`div` 2)

-- Punto A.2.b

muffinMagico :: Padrino
muffinMagico unChico = foldr ($) unChico (deseos unChico)

-- Punto B

-- Punto B.1

type Condicion = Chico -> Bool

-- Punto B.1.a

tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad unaHabilidad = elem unaHabilidad . habilidades

esSuperMaduro :: Condicion
esSuperMaduro unChico = esMayorDeEdad unChico && tieneHabilidad "manejar" unChico

esMayorDeEdad :: Chico -> Bool
esMayorDeEdad = (> 18) . edad

-- Punto B.2

data Chica = Chica {
  nombreChica :: String,
  condicion   :: Condicion
} deriving Show

trixie :: Chica
trixie = Chica "Trixie Tang" noEsTimmy

vicky :: Chica
vicky = Chica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

noEsTimmy :: Condicion
noEsTimmy = (/= "Timmy") . nombre

-- Punto B.2.a

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica [unPretendiente] = unPretendiente
quienConquistaA unaChica (unPretendiente : otrosPretendientes)
  | conquistaA unaChica unPretendiente = unPretendiente
  | otherwise                          = quienConquistaA unaChica otrosPretendientes

conquistaA :: Chica -> Chico -> Bool
conquistaA = condicion

-- Punto B.2.b

-- > Chica "Sra. Turner" (tieneHabilidad "cocinar")

-- Punto C

-- Punto C.1

infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules = map nombre . filter tieneDeseoProhibido

tieneDeseoProhibido :: Chico -> Bool
tieneDeseoProhibido unChico = any (esDeseoProhibidoPara unChico) . deseos $ unChico

esDeseoProhibidoPara :: Chico -> Deseo -> Bool
esDeseoProhibidoPara unChico unDeseo = tieneHabilidadesProhibidas $ unDeseo unChico

tieneHabilidadesProhibidas :: Chico -> Bool
tieneHabilidadesProhibidas = any esHabilidadProhibida . take 5 . habilidades

esHabilidadProhibida :: Habilidad -> Bool
esHabilidadProhibida unaHabilidad = elem unaHabilidad habilidadesProhibidas

habilidadesProhibidas :: [Habilidad]
habilidadesProhibidas = ["enamorar", "matar", "dominar el mundo"]

-- Punto D

{-
  Indicar donde se utilizó y para qué:

  - la función composición

    Se usa varias veces a lo largo del parcial; un caso a destacar es el de `habilidadesDeJugarAlNeedForSpeed`, donde se usa para crear una nueva
    función para pasarle al map, sin tener que darle un nombre a dicha función, lo cual puede resultar dificil.

  - funciones de orden superior (creadas)

    En las funciones mapEdad y mapHabilidad; se usan para evitar la repetición de lógica de "modificar un atributo del chico".

  - aplicación parcial

    Se usa muchas veces a lo largo del parcial; por ejemplo en `tieneHabilidadesProhibidas` para crear funciones que puedan ser compuestas.

  - listas infinitas: dar ejemplos de consultas que funcionen y que no, donde aparezcan estas listas

    Se usa una lista infinita en `habilidadesDeJugarAlNeedForSpeed` para crear la lista de habilidades de jugar a todas las versiones de need for speed.

    Consultas que funcionarían:
    > nombre . serGrosoEnNeedForSpeed $ timmy
    > take 10 . habilidades . serGrosoEnNeedForSpeed $ timmy
    > tieneHabilidad "mirar television" . serGrosoEnNeedForSpeed $ timmy

    Consultas que no funcionarían:
    > length . habilidades . serGrosoEnNeedForSpeed $ timmy
    > last   . habilidades . serGrosoEnNeedForSpeed $ timmy
    > tieneHabilidad "manejar" . serGrosoEnNeedForSpeed $ timmy
-}
