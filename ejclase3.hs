import Text.Show.Functions ()

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
casaDeHomero = ("Casa de Homero", 1000)

mansionBurns :: Propiedad
mansionBurns = ("Mansion Sr. Burns", 5000)

deptoPatySelma :: Propiedad
deptoPatySelma = ("Rancho de Paty y Selma", 500)

jugador1 = UnJugador "Carolina" 500 "Accionista" [] [pasarPorElBanco]
jugador2 = UnJugador "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = (cambiarTactica "Comprador compulsivo" . agregarDinero 40) unJugador

agregarDinero :: Dinero -> Jugador -> Jugador
agregarDinero dinero unJugador = unJugador { cantDinero = cantDinero unJugador + dinero}

cambiarTactica :: Tactica -> Jugador -> Jugador
cambiarTactica unaTactica unJugador = unJugador { tactica = unaTactica}

enojarse :: Accion
enojarse unJugador = (gritar . agregarDinero 50) unJugador

gritar :: Accion
gritar unJugador = unJugador { nombre = "AHHHH" ++ nombre unJugador}

{-subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador
        | tactica unJugador == "Accionsta" = adquirirPropiedad unaPropiedad unJugador
        | tactica unJugador == "Comprador compulsivo" = adquirirPropiedad unaPropiedad unJugador
        | otherwise = unJugador
        -}
    

adquirirPropiedad :: Propiedad -> Jugador -> Jugador
adquirirPropiedad unaPropiedad unJugador = 
    unJugador { cantDinero = cantDinero unJugador - (snd unaPropiedad), propiedadesCompradas = propiedadesCompradas unJugador ++ [unaPropiedad]}




