import Text.Show.Functions()

type Tarea = Heroe -> Heroe

data Artefacto = Artefacto{
    
    nombre :: String,
    rareza :: Int
}deriving Show

data Heroe = Heroe{

    epiteto :: String,
    nombreHeroe :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}deriving Show

lanzaDelOlimpo = Artefacto "Lanza del Olimpo" 500
xiphos = Artefacto "Xiphos" 50 
relampagoDeZeus = Artefacto "Relampago de Zeus" 500

heroe1 = Heroe "" "Zeus" 600 [lanzaDelOlimpo,xiphos] []

ponerEpiteto :: String -> Heroe -> Heroe
ponerEpiteto unEpiteto unHeroe = unHeroe{epiteto = unEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto unHeroe = unHeroe{artefactos = unArtefacto : artefactos unHeroe }

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe
        | reconocimiento unHeroe > 1000 = ponerEpiteto "El mitico" unHeroe
        | reconocimiento unHeroe >= 500 = ponerEpiteto "El magnifico" . agregarArtefacto lanzaDelOlimpo $ unHeroe
        | reconocimiento unHeroe > 100 = ponerEpiteto "Hoplita" . agregarArtefacto xiphos $ unHeroe
        | otherwise = unHeroe

-- TAREAS
-- funciones auxiliares generales tareas

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea unaTarea unHeroe = unHeroe{tareas = unaTarea : tareas unHeroe}

realizarTarea :: Tarea -> Heroe -> Heroe
realizarTarea unaTarea unHeroe = unaTarea unHeroe

modificarReconocimiento :: Int -> Heroe -> Heroe
modificarReconocimiento cantidad heroe = heroe{reconocimiento = reconocimiento heroe + cantidad}

-- Tarea 1
encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto heroe =  agregarArtefacto unArtefacto . modificarReconocimiento (rareza unArtefacto) $ heroe

-- Tarea 2
modificarArtefacto :: Int -> Artefacto -> Artefacto
modificarArtefacto cantidad artefacto = artefacto{rareza = rareza artefacto + cantidad}

modificarArtefactos :: Int -> Heroe -> Heroe
modificarArtefactos cantidad heroe = heroe{artefactos = map (modificarArtefacto cantidad) $ artefactos heroe} 

artefactosRaros :: Heroe -> Heroe
artefactosRaros heroe = heroe{artefactos = filter esArtefactoRaro (artefactos heroe)} 

esArtefactoRaro :: Artefacto -> Bool
esArtefactoRaro unArtefacto = rareza unArtefacto > 1000

escalarOlimpo :: Tarea
escalarOlimpo heroe =  agregarArtefacto relampagoDeZeus . artefactosRaros . modificarArtefactos ((length (artefactos heroe)) * 2) . modificarReconocimiento 500 $ heroe

-- Tarea 3

nivelDeGroso :: Int -> String
nivelDeGroso cuadras = "Gros" ++ concat (replicate cuadras "o")

ayudarACruzar :: Int -> Tarea
ayudarACruzar cuadras heroe = ponerEpiteto (nivelDeGroso cuadras) heroe

-- Tarea 4

data Bestia = Bestia{

    debilidadReconocimiento :: Int,
    debilidadArtefacto :: Artefacto
}deriving Show

--aprovecharDebilidad :: Heroe -> Bestia -> Bool
--aprovecharDebilidad heroe bestia = reconocimiento heroe >= debilidadReconocimiento bestia || debilidadDeArtefacto  heroe bestia

--debilidadDeArtefacto :: Heroe -> Bestia -> Bool
--debilidadArtefacto heroe bestia = 
 
 obtenerNombreArtefacto :: Artefacto -> String





