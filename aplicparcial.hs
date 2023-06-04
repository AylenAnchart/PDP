{-Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese 
número el 1. -}

siguiente :: Int -> Int
siguiente = (1+)

{-Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número, ej: 
Main> mitad 5
2.5
-}

mitad :: Float -> Float
mitad = (/2)

{-Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa. 
Main> inversa 4
0.25
Main> inversa 0.5
2.0
-}

inversa :: Float -> Float
inversa = (1/)

{-Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el 
número es positivo y false en caso contrario. 
Main> esNumeroPositivo (-5)
False
Main> esNumeroPositivo 0.99
True -}

esNumeroPositivo :: Float -> Bool
esNumeroPositivo = (>=0)


