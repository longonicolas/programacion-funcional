aplicarDescuento :: Num a => a -> a -> a
aplicarDescuento precio descuento = precio - descuento

aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio precio envio = precio + envio

precioTotal :: Num a => a -> a -> a -> a -> a
precioTotal precio descuento envio cantidad = aplicarCostoDeEnvio ((aplicarDescuento precio descuento) * cantidad) envio

esProductoCodiciado :: String -> Bool
esProductoCodiciado unProducto = (length unProducto) > 10

esProductoCorriente :: String -> Bool
esProductoCorriente unProducto = elem (head unProducto) "aeiou"

esProductoDeLujo :: String -> Bool
esProductoDeLujo unProducto = elem 'x' unProducto || elem 'z' unProducto

esProductoDeElite :: String -> Bool
esProductoDeElite unProducto = esProductoDeLujo unProducto && esProductoCodiciado unProducto && not (esProductoCorriente unProducto)

descodiciarProducto :: String -> String
descodiciarProducto unProducto = (reverse.drop 2.reverse) unProducto

esEntregaSencilla :: String -> Bool
esEntregaSencilla dia = (length dia) `mod` 2 == 0

productoXL :: String -> String
productoXL unProducto = unProducto ++ "XL"


