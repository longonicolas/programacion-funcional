{- Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es
mayor al segundo devuelve la suma, si el segundo le lleva más de 10 al primero
devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega
a llevarle 10, devuelve el producto.
-}

type Tupla = (Int, Int)

prueba1 :: Tupla
prueba2 :: Tupla
prueba1 = (50,20)
prueba2 = (30,40)

cuentaBizarra :: Tupla -> Int
cuentaBizarra unaTupla 
        | fst unaTupla > snd unaTupla = fst unaTupla + snd unaTupla
        | (snd unaTupla) - 10 > fst unaTupla = snd unaTupla - fst unaTupla
        | snd unaTupla > fst unaTupla = snd unaTupla * fst unaTupla

