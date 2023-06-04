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

-- funciones auxiliares punto 1

cambiarCansancio delta turista = turista{cansancio = cansancio turista + delta}

cambiarEstres :: Int -> Turista -> Turista
cambiarEstres delta turista = turista{estres = estres turista + delta}

aprenderIdioma idioma turista = turista{idiomas = idiomas turista ++ [idioma]}

viajarAcompaniado turista = turista{modoDeViaje = False}

intensidad minutos = div minutos 4

cambiarEstresPorcentual porcentaje unTurista = unTurista{estres = estres unTurista + div (porcentaje * estres unTurista) 100}

-- PUNTO 1

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


-- PUNTO 2

hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion unTurista = cambiarEstresPorcentual (-10) . excursion $ unTurista

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice unTurista excursion = deltaSegun indice (hacerUnaExcursion excursion unTurista) unTurista  


excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa unTurista =  (>= 1) . deltaExcursionSegun (length . idiomas) unTurista

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes unTurista listaDeExcursiones = filter (esDesestresante unTurista) $ listaDeExcursiones                                                  

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista excursion = (<= -3) . deltaExcursionSegun estres unTurista $ excursion

-- PUNTO 3

type Tour = [Excursion]

tourCompleto :: Tour
tourCompleto = [caminar 20, apreciarElPaisaje "cascada", caminar 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco "tranquila", excursion, caminar 120]

islaVecina :: String -> Tour
islaVecina marea
                | marea == "fuerte" = [paseoEnBarco marea, apreciarElPaisaje "lago", paseoEnBarco marea]
                | otherwise = [paseoEnBarco marea, irALaPlaya, paseoEnBarco marea]


hacerUnTour :: Turista -> Tour -> Turista
hacerUnTour unTurista unTour = foldr hacerUnaExcursion (cambiarEstres (length unTour) unTurista) unTour

tourConvincente :: Turista -> [Tour] -> Bool
tourConvincente unTurista tours = any (esConvincente unTurista) tours

esConvincente :: Turista -> Tour -> Bool
esConvincente unTurista unTour = any (terminaAcompaniado unTurista) . excursionesDesestresantes unTurista $ unTour

terminaAcompaniado :: Turista -> Excursion -> Bool
terminaAcompaniado unTurista excursion = not . modoDeViaje $ (hacerUnaExcursion excursion unTurista)

--efectividadDelTour :: Tour -> [Turista] -> [Turista]
--efectividadDelTour unTour grupoDeTuristas = filter (flip (tourConvincente unTour grupoDeTuristas))

obtenerEspiritualidad :: Turista -> Tour -> Int
obtenerEspiritualidad unTurista unTour =  sum . map (sumarPerdidas unTurista) $ unTour

sumarPerdidas :: Turista -> Excursion -> Int
sumarPerdidas unTurista excursion = (-1) * (deltaExcursionSegun estres unTurista excursion + deltaExcursionSegun cansancio unTurista excursion)
