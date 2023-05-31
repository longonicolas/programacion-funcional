type Cansancio = Int
type Estres = Int
type ViajaSolo = Bool
type Idioma = String
type Idiomas = [Idioma]
type Excursion = Turista -> Turista

data Turista = Turista{

    cansancio :: Cansancio,
    estres :: Estres,
    modoDeViaje :: ViajaSolo,
    idiomas :: Idiomas

    }deriving Show

ana :: Turista
ana = Turista 0 21 False ["Espaniol"]

beto :: Turista
beto = Turista 15 15 True []

cathi :: Turista
cathi = Turista 15 15 True ["Catalan"]

-- funciones auxiliares

cambiarCansancio delta turista = turista{cansancio = cansancio turista + delta}

cambiarEstres delta turista = turista{estres = estres turista + delta}

aprenderIdioma idioma turista = turista{idiomas = idiomas turista ++ [idioma]}

viajarAcompaniado turista = turista{modoDeViaje = False}

intensidad minutos = div minutos 4

cambiarEstresPorcentual porcentaje unTurista = unTurista{estres = estres unTurista + div (porcentaje * estres unTurista) 100}

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

-- EXCURSIONES

irALaPlaya :: Excursion
irALaPlaya unTurista 
        | modoDeViaje unTurista = cambiarCansancio (-5) unTurista
        | otherwise = cambiarEstres (-1) unTurista

apreciarElPaisaje :: String -> Excursion
apreciarElPaisaje paisaje unTurista = cambiarEstres (-length paisaje) unTurista

salirAHablarUnIdioma :: Idioma -> Excursion
salirAHablarUnIdioma idioma = viajarAcompaniado . aprenderIdioma idioma 

caminar :: Int -> Excursion
caminar tiempoCaminado = cambiarCansancio (intensidad tiempoCaminado) . cambiarEstres (-intensidad tiempoCaminado) 

paseoEnBarco :: String -> Excursion
paseoEnBarco marea unTurista
        | marea == "fuerte" =  cambiarEstres (6) $ cambiarCansancio (10) unTurista
        | marea == "tranquila" = caminar 10 (salirAHablarUnIdioma "Aleman" (apreciarElPaisaje "mar" unTurista))
        | otherwise = unTurista

hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion = cambiarEstresPorcentual (-10) . excursion 

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice unTurista excursion = deltaSegun indice (hacerUnaExcursion excursion unTurista) unTurista  


excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa unTurista excursion = (deltaExcursionSegun (length . idiomas) (unTurista) (hacerUnaExcursion excursion) ) >= 1
