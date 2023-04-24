type Edad = Int
type Crecimiento = Int
type Altura = Int

crecimientoAnual :: Edad -> Crecimiento
crecimientoAnual unaEdad    
        | unaEdad >= 0 && unaEdad <= 10 = 24 - (unaEdad * 2)
        | unaEdad > 10 && unaEdad <= 15 = 4
        | unaEdad > 15 && unaEdad <= 17 = 2
        | unaEdad > 17 && unaEdad <= 19 = 1
        | otherwise = 0

crecimientoEntreEdades :: Edad -> Edad -> Crecimiento
crecimientoEntreEdades edad1 edad2 = sum (map crecimientoAnual (enumFromTo (edad1) (edad2-1)))

alturasEnUnAnio :: Edad -> [Altura] -> [Altura]
alturasEnUnAnio unaEdad listaDeAlturas = map (+crecimientoAnual unaEdad) listaDeAlturas

edadEnDeterminadoAnio :: Altura -> Edad -> Edad -> Altura
edadEnDeterminadoAnio alturaActual edadActual edadDeseada = alturaActual + (crecimientoEntreEdades edadActual edadDeseada)

alturaEnEdades :: Altura -> Edad -> [Edad] -> [Altura]
alturaEnEdades altura edad listaDeEdades = map (edadEnDeterminadoAnio altura edad) listaDeEdades
