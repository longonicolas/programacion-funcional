import Text.Show.Functions ()

type Nombre = String
type Dinero = Int
type Precio = Int
type Tactica = String
type Propiedad = (Nombre, Precio)

data Jugador = UnJugador {nombre :: Nombre, cantDinero :: Dinero, tactica :: Tactica, propiedadesCompradas :: [Propiedad]} deriving (Show, Eq)

jugador1 = UnJugador "Carolina" 500 "Accionista" []
jugador2 = UnJugador "Manuel" 500 "Oferente singular" []

type Accion = Jugador -> Jugador

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = (cambiarTactica "Comprador compulsivo" . agregarDinero 40) unJugador

agregarDinero :: Dinero -> Jugador -> Jugador
agregarDinero dinero unJugador = unJugador { cantDinero = cantDinero unJugador + dinero}

cambiarTactica :: Tactica -> Jugador -> Jugador
cambiarTactica unaTactica unJugador = unJugador { tactica = unaTactica}






