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
ana = Turista 0 21 False ["Español"]

beto :: Turista
cathi :: Turista

beto = Turista 15 15 True []
cathi = Turista 15 15 True ["Catalán"]

irALaPlaya :: Excursion
irALaPlaya unTurista 
        | modoDeViaje unTurista = unTurista{cansancio = cansancio unTurista - 5 }
        | otherwise = unTurista{estres = estres unTurista - 1}

apreciarElPaisaje :: String -> Excursion
apreciarElPaisaje paisaje unTurista = unTurista{ estres = estres unTurista - (length paisaje)}

salirAHablarUnIdioma :: Idioma -> Excursion
salirAHablarUnIdioma idioma unTurista = unTurista{ idiomas = idiomas unTurista ++ [idioma], modoDeViaje = not $ modoDeViaje unTurista}

caminar :: Int -> Excursion
caminar tiempoCaminado unTurista = unTurista{ cansancio = cansancio unTurista + (div tiempoCaminado 4), estres = estres unTurista - ((div tiempoCaminado 4))}

paseoEnBarco :: String -> Excursion
paseoEnBarco marea unTurista
        | marea == "fuerte" = unTurista{ estres = estres unTurista + 6, cansancio = cansancio unTurista + 10}
        | marea == "tranquila" = caminar 10 (salirAHablarUnIdioma "Aleman" (apreciarElPaisaje "mar" unTurista))
        | otherwise = unTurista
