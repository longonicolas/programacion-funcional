import Text.Show.Functions () --reemplaza al Show en el data

type Nombre = String
type Dinero = Int
type Precio = Int
type Tactica = String
type Propiedad = (Nombre, Precio)
type Accion = Jugador -> Jugador
type Acciones = [Accion]

data Jugador = UnJugador{ 
    nombre :: Nombre,
    cantDinero :: Dinero,
    tactica :: Tactica,
    propiedadesCompradas :: [Propiedad],
    acciones :: Acciones 
}deriving Show

casaDeHomero :: Propiedad
casaDeHomero = ("Casa de Homero", 100)

mansionBurns :: Propiedad
mansionBurns = ("Mansion Sr. Burns", 500)

deptoPatySelma :: Propiedad
deptoPatySelma = ("Rancho de Paty y Selma", 50)

jugador1 = UnJugador "Carolina" 500 "Accionista" [casaDeHomero,mansionBurns] [pasarPorElBanco]
jugador2 = UnJugador "Manuel" 500 "Oferente singular" [deptoPatySelma] [pasarPorElBanco, enojarse]

-- funciones auxiliares

--auxiliares para pasarPorElBanco
cambiarDinero :: Dinero -> Jugador -> Jugador
cambiarDinero dinero unJugador = unJugador { cantDinero = cantDinero unJugador + dinero}

cambiarTactica :: Tactica -> Jugador -> Jugador
cambiarTactica unaTactica unJugador = unJugador { tactica = unaTactica}

--auxiliares para subastar
adquirirPropiedad :: Propiedad -> Jugador -> Jugador
adquirirPropiedad unaPropiedad unJugador = 
    unJugador { cantDinero = cantDinero unJugador - (snd unaPropiedad), propiedadesCompradas = propiedadesCompradas unJugador ++ [unaPropiedad]}

--auxiliares para cobrarAlquileres
esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = (snd unaPropiedad) < 150

precioAlquileres :: Propiedad -> Int
precioAlquileres unaPropiedad
        | esPropiedadBarata unaPropiedad = 10
        | otherwise                      = 20

ingresosPorAlquileres :: Jugador -> Int
ingresosPorAlquileres unJugador = sum . map precioAlquileres $ propiedadesCompradas unJugador

-- Acciones

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = (cambiarTactica "Comprador compulsivo" . cambiarDinero 40) unJugador

enojarse :: Accion
enojarse unJugador = (gritar . cambiarDinero 50) unJugador

gritar :: Accion
gritar unJugador = unJugador { nombre = "AHHHH" ++ nombre unJugador}

subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador
        | tactica unJugador == "Accionista" = adquirirPropiedad unaPropiedad unJugador
        | tactica unJugador == "Oferente singular" = adquirirPropiedad unaPropiedad unJugador
        | otherwise = unJugador

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = cambiarDinero (ingresosPorAlquileres unJugador) unJugador

pagarAAcionistas :: Accion
pagarAAcionistas unJugador
        | tactica unJugador == "Accionista" = unJugador { cantDinero = cantDinero unJugador + 200}
        | otherwise = unJugador { cantDinero = cantDinero unJugador - 100}
