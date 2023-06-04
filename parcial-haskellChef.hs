import Text.Show.Functions ()

type Nombre = String
type Truco = Plato -> Plato
type Componente = (Ingrediente,Peso)
type Ingrediente = String
type Peso = Int

data Plato = UnPlato{

    dificultad :: Int,
    componentes :: [Componente]
}deriving Show

data Participante = UnParticipante{

    nombre :: Nombre,
    listaDeTrucos :: [Truco],
    plato :: Plato
}deriving Show

platoPrueba = UnPlato 8 [("leche",5),("agua",20),("miel",5),("agua",20),("miel",5),("agua",20)]
platoPrueba2 = UnPlato 8 [("miel",5),("agua",20),("azucar",12),("sal",5), ("leche",100)]
-- PARTE A

agregarComponente :: Ingrediente -> Peso -> Plato -> Plato
agregarComponente ingrediente peso unPlato = unPlato{ componentes = (ingrediente,peso) : componentes unPlato }

endulzar :: Peso -> Truco
endulzar unPeso unPlato = agregarComponente "azucar" unPeso unPlato

salar :: Peso -> Truco
salar unPeso unPlato = agregarComponente "sal" unPeso unPlato

darSabor :: Peso -> Peso -> Truco
darSabor pesoSal pesoAzucar unPlato = (endulzar pesoAzucar . salar pesoSal) unPlato

duplicarComponentes :: Plato -> Plato
duplicarComponentes unPlato = unPlato{ componentes = map duplicarComponente $ componentes unPlato}

duplicarComponente :: Componente -> Componente
duplicarComponente (a,b) = (a, 2 * b)

duplicarPorcion :: Truco
duplicarPorcion unPlato = duplicarComponentes unPlato

reducirDificultad :: Plato -> Plato
reducirDificultad unPlato = unPlato{ dificultad = 5}

quitarComponentes :: Plato -> Plato 
quitarComponentes unPlato = unPlato{ componentes = filter (\(_,peso) -> peso > 10) $ componentes unPlato}

simplificar :: Truco
simplificar unPlato
        | esComplejo' unPlato = (reducirDificultad . quitarComponentes) unPlato
        | otherwise = unPlato

productosAnimales :: [Ingrediente]
productosAnimales = ["huevo","carne","leche","queso","manteca"]

encontrarIngrediente :: Plato -> Ingrediente -> Bool
encontrarIngrediente unPlato ingrediente = any (\ (a,_) -> a == ingrediente) $ componentes unPlato

esVegano :: Plato -> Bool
esVegano unPlato = not . any (encontrarIngrediente unPlato) $ productosAnimales

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not $ encontrarIngrediente unPlato "harina"

esComplejo :: Plato -> Bool
esComplejo unPlato = (length (componentes unPlato) > 5) && (dificultad unPlato > 7)

esComplejo' :: Plato -> Bool
esComplejo' unPlato =  ((>7) . dificultad $ unPlato ) &&  ((>5) . length . componentes $ unPlato)

obtenerComponente :: Ingrediente -> [Componente] -> [Componente]
obtenerComponente ingrediente listaDeComponentes = filter (\ (a,_) -> a == ingrediente) listaDeComponentes

gramosDeIngrediente :: Ingrediente -> Plato -> Peso
gramosDeIngrediente ingrediente unPlato = sum . (map snd . obtenerComponente ingrediente) $ componentes unPlato

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = gramosDeIngrediente "sal" unPlato > 2

-- PARTE B

pepeRonccino :: Participante
pepeRonccino = UnParticipante "pepe Ronccino" [darSabor 2 5, simplificar, duplicarPorcion] platoEspecial

platoEspecial :: Plato
platoEspecial = UnPlato 8 [("sal",10),("panceta",8),("jamon",10),("huevo",3),("arroz",30),("limon",3)] 

-- PARTE C

aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco unPlato unTruco = unTruco unPlato

aplicarTrucos :: Plato -> [Truco] -> Plato
aplicarTrucos unPlato listaDeTrucos = foldl aplicarTruco unPlato listaDeTrucos

cocinar :: Participante -> Plato
cocinar unParticipante = aplicarTrucos (plato unParticipante) (listaDeTrucos unParticipante)

totalGramos :: Plato -> Int
totalGramos unPlato = sum . map snd . componentes $ unPlato

esMejorQue :: Plato -> Plato -> Bool 
esMejorQue plato1 plato2 = dificultad plato1 > dificultad plato2 && totalGramos plato1 < totalGramos plato2

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante parti1 parti2
        | esMejorQue (cocinar parti1) (cocinar parti2) = parti1
        | otherwise = parti2

participanteEstrella :: [Participante] -> Participante
participanteEstrella listaDeParticipantes = foldl1 mejorParticipante listaDeParticipantes  

-- PARTE D

platoPlatinum :: Plato
platoPlatinum = UnPlato 10 listaDeComponentesPlatinum

listaDeComponentesPlatinum :: [Componente]
listaDeComponentesPlatinum = map (\unNumero -> ("Ingrediente " ++ show unNumero, unNumero)) [1..]
