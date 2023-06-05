{-Nota previa 
Se base en el concepto matemático de composición de funciones. 
g(f(x)) = (g o f) x 
La imagen de f(x) tiene que coincidir con el dominio de g(x) 

Acá la notación es (g . f) x . Veamos un Ejemplo: 
g n = even n 
f n = 3 + n 

Main> (g . f) 4 
False 
Main> (g . f) 3
True
-}

{-Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando aplicación parcial y composición.-}

{-Planteo anterior: esMultiplo:: Int -> Int -> Bool
esMultiplo nro1 nro2 = mod nro2 nro1 == 0 -}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe = ((== 0) . ). mod
