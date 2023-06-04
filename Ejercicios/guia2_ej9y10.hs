incrementoMCuadradoN :: Int -> Int -> Int
incrementoMCuadradoN a b = ((+b).(^2)) a

esResultadoPar :: Int -> Int -> Bool
esResultadoPar a b =  mod ((^b) a) 2 == 0
