{- Representamos las notas que se sacó un alumno en dos parciales mediante un par
(nota1,nota2), p.ej. un patito en el 1ro y un 7 en el 2do se representan mediante el
par (2,7).
A partir de esto:
a. Definir la función esNotaBochazo, recibe un número y devuelve True si no
llega a 6, False en caso contrario. No vale usar guardas.
b. Definir la función aprobo, recibe un par e indica si una persona que se sacó
esas notas aprueba. Usar esNotaBochazo.
c. Definir la función promociono, que indica si promocionó, para eso tiene las
dos notas tienen que sumar al menos 15 y además haberse sacado al menos
7 en cada parcial.
d. Escribir una consulta que dado un par indica si aprobó el primer parcial,
usando esNotaBochazo y composición. La consulta tiene que tener esta
forma (p.ej. para el par de notas (5,8))-}

type Notas = (Int, Int)

esNotaBochazo :: Int -> Bool
esNotaBochazo unNumero = unNumero < 6

aprobo :: Notas -> Bool
aprobo parDeNotas = not ((esNotaBochazo.fst) parDeNotas) && not ((esNotaBochazo.snd) parDeNotas) 

promociono :: Notas -> Bool
promociono parDeNotas = (fst parDeNotas) + (snd parDeNotas) == 15 && fst parDeNotas >= 7 
                        && snd parDeNotas >= 7

