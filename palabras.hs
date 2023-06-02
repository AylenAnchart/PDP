{-
Palabras
[ Listas, Listas por comprensión/orden superior/recursividad ]
a) Obtener la cantidad de vocales que tiene una palabra

b) Devolver si es cierto que una frase tiene más consonantes que vocales

c) Armar la sigla de una frase compuesta por varias palabras
-}

--Cantidad de vocales de una palabra

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiou"

obtenerVocales :: [Char] -> [Char]
obtenerVocales = filter esVocal

cantVocales :: [Char] -> Int
cantVocales = length.obtenerVocales

obtenerConsonantes :: [Char] -> [Char]
obtenerConsonantes = filter (not.esVocal)

cantConsonante :: [Char] -> Int
cantConsonante = length.obtenerConsonantes