import Text.Show.Functions

data Persona = Persona{

nombre :: String,
calorias :: Int,
hidratacion :: Int,
tiempoDisponible :: Int,
equipamientos :: [String]

}deriving Show

type Ejercicio = Persona -> Persona

carla = Persona "Carla" 200 40 120 ["pesa"]
carla1 = Persona "Car" 200 60 110 ["pesa"]
carla2 = Persona "Ca" 200 70 120 ["pesa"]

abdominales :: Int -> Ejercicio
abdominales reps unaPersona = modificarCalorias (-8 * reps) unaPersona

modificarCalorias :: Int -> Persona -> Persona
modificarCalorias cantidad unaPersona = unaPersona{calorias = calorias unaPersona + cantidad}

flexiones :: Int -> Ejercicio
flexiones cantidad unaPersona =  modificarHidratacion (porcentualHidratacion cantidad). modificarCalorias ((-16) * cantidad) $ unaPersona

porcentualHidratacion :: Int -> Int
porcentualHidratacion cantidad = (div cantidad 5) * (-1)

modificarHidratacion :: Int -> Persona -> Persona
modificarHidratacion cantidad unaPersona = unaPersona{hidratacion = hidratacion unaPersona + cantidad}

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas reps pesoPesa unaPersona
        | tiene "pesa" unaPersona = modificarHidratacion ((div reps (-10)) * pesoPesa) . modificarCalorias ((-32) * reps) $ unaPersona
        | otherwise = unaPersona

tiene :: String -> Persona -> Bool
tiene equipamiento unaPersona = elem equipamiento (equipamientos unaPersona)

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson = id

type Accion = Persona -> Persona

renovarEquipo :: Accion
renovarEquipo unaPersona = editarEquipamiento (renovar (equipamientos unaPersona)) unaPersona

editarEquipamiento :: [String] -> Persona -> Persona
editarEquipamiento nuevoEquipamiento unaPersona = unaPersona{equipamientos = nuevoEquipamiento}

renovar :: [String] -> [String]
renovar equipo = map ("Nuevo " ++) equipo

volverseYoguista :: Accion
volverseYoguista unaPersona = modificarHidratacionPorcenctual . editarEquipamiento ["colchoneta"] . modificarCalorias (div (calorias unaPersona) (-2)) $ unaPersona

modificarHidratacionPorcenctual :: Persona -> Persona
modificarHidratacionPorcenctual unaPersona
        | hidratacion unaPersona > 50 = modificarHidratacion (100 - (hidratacion unaPersona)) unaPersona
        | otherwise = modificarHidratacion (hidratacion unaPersona) unaPersona

volverseBodyBuilder :: Accion
volverseBodyBuilder unaPersona
                | esBodyBuilder unaPersona = modificarCalorias (2 * (calorias unaPersona)) . modificarNombre "BB" $ unaPersona
                | otherwise = unaPersona

esBodyBuilder :: Persona -> Bool
esBodyBuilder unaPersona = all (== "pesa") .  equipamientos $ unaPersona

modificarNombre :: String -> Persona -> Persona
modificarNombre agregado unaPersona = unaPersona{nombre = nombre unaPersona ++ agregado}

comerseUnSandwich :: Accion
comerseUnSandwich unaPersona = modificarHidratacion (100 - (hidratacion unaPersona)) . modificarCalorias 500 $ unaPersona

-- PARTE B

data Rutina = Rutina{
        duracion :: Int,
        ejercicios :: [Ejercicio]
}deriving Show

rutinarda = Rutina 60 [abdominales 10, levantarPesas 10 5]

puedeHacerRutina :: Persona -> Rutina -> Bool
puedeHacerRutina unaPersona rutina = tiempoDisponible unaPersona > duracion rutina

hacerRutina :: Rutina -> Persona -> Persona
hacerRutina rutina unaPersona
        | puedeHacerRutina unaPersona rutina = foldr hacerEjercicio unaPersona (ejercicios rutina)
        | otherwise = unaPersona

hacerEjercicio :: Ejercicio -> Persona -> Persona
hacerEjercicio ejercicio unaPersona = ejercicio unaPersona 

esPeligrosa :: Persona -> Rutina -> Bool
esPeligrosa unaPersona rutina = calorias (hacerRutina rutina unaPersona) < 50 && hidratacion (hacerRutina rutina unaPersona) < 10

esBalanceada :: Persona -> Rutina -> Bool
esBalanceada unaPersona rutina = hidratacion (hacerRutina rutina unaPersona) > 80 && (calorias (hacerRutina rutina unaPersona) <  div (calorias unaPersona) 2)

elAbominableAbdominal :: Rutina
elAbominableAbdominal = Rutina 60 [absInfinitos [1..]]

absInfinitos :: [Int] -> Ejercicio
absInfinitos [] unaPersona = unaPersona
absInfinitos (reps:repsRestantes) unaPersona = absInfinitos repsRestantes (abdominales reps unaPersona)


elAbominableAbdominal' :: Rutina
elAbominableAbdominal' = Rutina 60 (map abdominales [1..])

-- PARTE C

seleccionarGrupoDeEjercicio :: Persona -> [Persona] -> [Persona]
seleccionarGrupoDeEjercicio unaPersona candidatos = filter (mismoTiempoDispo unaPersona) $ candidatos

mismoTiempoDispo :: Persona -> Persona -> Bool
mismoTiempoDispo persona1 persona2 = tiempoDisponible persona1 == tiempoDisponible persona2

type Indices = (Int,Int)

promedioDeRutina :: Rutina -> [Persona] -> Indices
promedioDeRutina rutina grupo = (promedioGrupo . rutinaGrupal rutina $ grupo, indiceHidratacionGrupal . rutinaGrupal rutina $ grupo)

rutinaGrupal :: Rutina -> [Persona] -> [Persona]
rutinaGrupal rutina grupo = map (hacerRutina rutina) grupo

promedioGrupo :: [Persona] -> Int
promedioGrupo grupo = div (sum (map calorias grupo)) (length grupo)

indiceHidratacionGrupal :: [Persona] -> Int
indiceHidratacionGrupal grupo = div (sum (map hidratacion grupo)) (length grupo)