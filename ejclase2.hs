elVisitante = ("El Visitante","Stephen King",592)
shingeki = ("Shingeki","Hajime",120)
fundacion = ("Fundacion","Isaac Asimov",230)
sandman = ("Sandman","Neil Gaiman",105)
eragon1 = ("Eragon","Christoper Paolini",544)
eragon2 = ("Eldest","Christoper Paolini",704)
eragon3 = ("Brisignr","Christoper Paolini",700)
eragon4 = ("Legado", "Christoper Paolini",811)

biblioteca :: [(String,String,Int)]
biblioteca = [elVisitante,shingeki,fundacion,sandman,eragon1,eragon2,eragon3,eragon4]

devolverHojasLibro (_, _, hojas) = hojas

listaDeHojas :: [(a,b,c)] -> [c]
listaDeHojas biblioteca = map devolverHojasLibro biblioteca

sumaCantHojas :: [(a,b,Int)] -> Int
sumaCantHojas biblioteca = sum (listaDeHojas biblioteca)

dividirPorCantLibros :: [(a,b,c)] -> Int
dividirPorCantLibros biblioteca = length biblioteca


promedioDeHojas :: [(a,b,Int)] -> Float
promedioDeHojas biblioteca = fromIntegral (sumaCantHojas biblioteca) / fromIntegral (dividirPorCantLibros biblioteca)
