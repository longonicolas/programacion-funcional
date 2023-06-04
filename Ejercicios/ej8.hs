fahrToCelsius :: Float -> Float
haceFrio :: Float -> Bool

fahrToCelsius gradoFahrenheit = (gradoFahrenheit - 32) * (5/9)

haceFrio gradoFar = fahrToCelsius gradoFar < 8

