elVisitante = ("El Visitante","Stephen King",592)
theShining = ("The Shining","Stephen King",301)
shingeki = ("Shingeki","Hajime",120)
fundacion = ("Fundacion","Isaac Asimov",230)
sandman = ("Sandman","Neil Gaiman",105)
eragon1 = ("Eragon","Christoper Paolini",544)
eragon2 = ("Eldest","Christoper Paolini",704)
eragon3 = ("Brisignr","Christoper Paolini",700)
eragon4 = ("Legado", "Christoper Paolini",811)

type Libro = (Titulo,Autor,Int)
type Autor = String
type Titulo = String

type Biblioteca = [(Libro)]

biblioteca :: Biblioteca
biblioteca = [elVisitante,shingeki,fundacion,sandman,eragon1,eragon2,eragon3,eragon4,theShining]

    -- PUNTO 1:  promedioDeHojas de nuestra biblioteca --

devolverHojasLibro :: Libro -> Int
devolverHojasLibro (_, _, hojas) = hojas

listaDeHojas :: Biblioteca -> [Int]
listaDeHojas biblioteca = map devolverHojasLibro biblioteca

sumaCantHojas :: Biblioteca -> Int
sumaCantHojas biblioteca = sum (listaDeHojas biblioteca)

promedioDeHojas :: Biblioteca -> Int
promedioDeHojas biblioteca =  div (sumaCantHojas biblioteca)  (length biblioteca)

    -- PUNTO 2: lecturaObligatoria, esto es así cuando es de Stephen King 
    --o de la saga de Eragon o es el ejemplar de Fundación de 230 páginas de Isaac Asimov  --

autor :: Libro -> Autor
autor (_,unAutor,_) = unAutor

type Saga = [Libro]

sagaEragon :: Saga
sagaEragon = [eragon1,eragon2,eragon3,eragon4]

lecturaObligatoria :: Libro -> Bool
lecturaObligatoria unLibro = esDeStephenKing unLibro 
                           || esDeSagaEragon unLibro 
                           || esFundacion unLibro

esDeStephenKing :: Libro -> Bool
esDeStephenKing unLibro = autor unLibro == "Stephen King"

esDeSagaEragon :: Libro -> Bool
esDeSagaEragon unLibro = elem unLibro sagaEragon

esFundacion :: Libro -> Bool
esFundacion unLibro = unLibro == fundacion

    -- PUNTO 3: Si la biblioteca es fantasiosa, es decir, si tiene algún libro de Christopher
                --Paolini o de Neil Gaiman. --

listaDeAutores :: Biblioteca -> [Autor]
listaDeAutores biblioteca = map autor biblioteca

bibliotecaFantasiosa :: Biblioteca -> Bool
bibliotecaFantasiosa biblioteca = elem "Neil Gaiman" (listaDeAutores biblioteca) 
                                || elem "Christopher Paolini" (listaDeAutores biblioteca)

    -- PUNTO 4: nombreDeLaBiblioteca, que es el nombre de todos los títulos juntos,
        --sacándole las vocales --

titulo :: Libro -> Titulo
titulo (unTitulo,_,_) = unTitulo

listaDeTitulos :: Biblioteca -> [Titulo]
listaDeTitulos biblioteca = map titulo biblioteca

nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca biblioteca = filter (`notElem` "aeiouAEIOU") (concat (listaDeTitulos biblioteca))

    -- PUNTO 5: Si tenemos una bibliotecaLigera, o sea, si todas sus lecturas tienen 40
    --          páginas o menos. --

esLecturaLigera :: Libro -> Bool
esLecturaLigera unLibro = (devolverHojasLibro unLibro) <= 40

bibliotecaLigera :: Biblioteca -> Bool
bibliotecaLigera biblioteca = all esLecturaLigera biblioteca


    {- PUNTO 6: El género de un libro. Diremos que:
                        - todos los libros escritos por Stephen King son de terror
                        - los libros escritos por un autor Japonés son manga
                        - los que tienen menos de 40 páginas son cómics
                        - el resto no están clasificados.-}

type Genero = String

autoresJaponeses :: [String]
autoresJaponeses = ["Hajime"]

esDeAutorJapones :: Libro -> Bool
esDeAutorJapones unLibro = (elem (autor unLibro)) autoresJaponeses

genero :: Libro -> Genero
genero unLibro 
    | esDeAutorJapones unLibro = "Manga"
    | esDeStephenKing unLibro = "Terror"
    | esLecturaLigera unLibro = "Comic"
    | otherwise = "Sin categoria"
